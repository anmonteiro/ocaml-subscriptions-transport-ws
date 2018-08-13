module type SubscriptionsManager = sig
  type t

  val subscriptions : t -> t (* (key * value) list *)

  val add : t -> string -> (unit -> unit) -> unit

  val create : int -> t

  val remove : t -> string -> unit

  val mem : t -> string -> bool

  val find_opt : t -> string -> (unit -> unit) option

  val iter : (string -> (unit -> unit) -> unit) -> t -> unit

  val clear : t -> unit
end

module type IO = sig
  type +'a t

  val return_unit : unit t

  val finalize : (unit -> 'a t) -> (unit -> unit t) -> 'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type Stream = sig
  type +'a io
  type 'a t

  val consume_stream : 'a t -> ('a -> unit) -> unit io

  val stream_destroy_fn : 'a t -> (unit -> unit)
end

module Make (Io : IO)
            (Stream : Stream with type 'a io = 'a Io.t)
            (SubscriptionsManager : SubscriptionsManager) = struct
  open Websocket
  module Json = Yojson.Basic.Util

  (* https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md *)
  type client_message =
    | Gql_connection_init
    | Gql_start
    | Gql_stop
    | Gql_connection_terminate
    (* not part of the protocol, used here to signal invalid messages *)
    | Invalid

  type server_message =
    | Gql_connection_error
    | Gql_connection_ack
    | Gql_data
    | Gql_error
    | Gql_complete
    (* | Gql_connection_keep_alive *)

  let client_message_of_payload payload_json =
    match payload_json |> (Json.member "type") |> Json.to_string with
    | "connection_init" -> Gql_connection_init
    | "start" -> Gql_start
    | "stop" -> Gql_stop
    | "connection_terminate" -> Gql_connection_terminate
    | _ -> Invalid

  let server_message_to_string = function
    | Gql_connection_error -> "connection_error"
    | Gql_connection_ack -> "connection_ack"
    | Gql_data -> "data"
    | Gql_error -> "error"
    | Gql_complete -> "complete"
  (* | Gql_connection_keep_alive -> "ka" *)

  (* let rec consume_pipe r cb =
    Async_kernel.Pipe.read r >>= function
    | `Ok x ->
      let Ok x | Error x = x in
      cb x;
      consume_pipe r cb
    | `Eof ->
      Core.eprintf "EOFY\n%!";
      Async_kernel.Deferred.unit
 *)

  let create_message ?(opcode=Frame.Opcode.Text) ?opId ?(payload=`Null) typ =
    let frame_payload = `Assoc [
        "type", `String (server_message_to_string typ);
        "id", begin match opId with
          | Some id -> `String id
          | None -> `Null end;
        "payload", payload
      ] in
    let json = Yojson.Basic.to_string frame_payload in
    Frame.create ~opcode ~content:json ()

  let on_recv mgr ?keepalive:_keepalive ~subscribe ~push_to_websocket frame =
    let push_to_websocket = Lazy.force push_to_websocket in
    let json = Yojson.Basic.from_string frame.Frame.content in
    match client_message_of_payload json with
    | Gql_connection_init ->
      (* TODO: allow a user-defined `on_connect` handler *)
      (* TODO: check for `graphql-ws` in the request headers, otherwise terminate connection *)
      push_to_websocket (Some (create_message Gql_connection_ack));
    | Gql_start ->
      let open Io in
      let opId = Json.(json |> member "id" |> to_string) in
      let payload_json = Json.member "payload" json in
      let query = Json.(payload_json |> member "query" |> to_string) in
      let variables =
        try
          Json.(payload_json |> member "variables" |> to_assoc)
        with | _ -> [] in
      let operation_name = Json.(payload_json |> member "operationName" |> to_string_option)
      in
      (* TODO: unsubscribe if there's a subscription with the same id *)
      let result = subscribe
        ~variables:(variables :> (string * Graphql_parser.const_value) list)
        ?operation_name
        query
      in
      let _ = result >>= (function
          | Error message ->
            let payload = `Assoc ["message", message] in
            push_to_websocket (Some (create_message ~payload ~opId Gql_error));
            Io.return_unit
          | Ok (`Response json) ->
            push_to_websocket (Some (create_message ~opId ~payload:json Gql_data));
            Io.return_unit
          | Ok (`Stream stream) ->
            let destroy = Stream.stream_destroy_fn stream
            in
            SubscriptionsManager.add mgr opId destroy;
            Io.finalize
              (fun () ->
                Stream.consume_stream stream
                 (fun x ->
                   let Ok x | Error x = x in
                    (*
                     * XXX: OGS doesn't yet have a way of effectively killing
                     * a stream – so if we've been asked to unsubscribe, don't
                     * push the execution result to the websocket.
                     *)
                    if SubscriptionsManager.mem mgr opId then
                      push_to_websocket (Some (create_message ~opId ~payload:x Gql_data))))
              (fun () ->
                 (if SubscriptionsManager.mem mgr opId then
                   push_to_websocket (Some (create_message ~opId Gql_complete)));
                   Io.return_unit))
      in ()
    | Gql_stop ->
      let opId = Json.(json |> member "id" |> to_string) in
      begin match SubscriptionsManager.find_opt mgr opId with
        | None -> ()
        | Some unsubscribe -> unsubscribe ()
      end;
      SubscriptionsManager.remove mgr opId;
    | Gql_connection_terminate ->
      SubscriptionsManager.iter (fun _ f -> f ()) mgr;
      SubscriptionsManager.clear mgr;
      push_to_websocket (Some (create_message ~opcode:Frame.Opcode.Close Gql_connection_error))
    | Invalid ->
      let opId = Json.(json |> member "id" |> to_string) in
      let payload = `Assoc ["message", `String "Invalid message type!"] in
      push_to_websocket (Some (create_message ~opId ~payload Gql_error));
end
