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

module Json = Yojson.Basic.Util

(* https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md *)
module Protocol = struct
  module Client = struct
    type t =
      | Gql_connection_init
      | Gql_start of
          { id : string
          ; query : string
          ; variables : (string * Graphql_parser.const_value) list
          ; operation_name : string option
          }
      | Gql_stop
      | Gql_connection_terminate

    let of_json json =
      match Json.(to_string (member "type" json)) with
      | "connection_init" ->
        Some Gql_connection_init
      | "start" ->
        let opId = Json.(json |> member "id" |> to_string) in
        let payload_json = Json.member "payload" json in
        let query = Json.(payload_json |> member "query" |> to_string) in
        let variables =
          try Json.(payload_json |> member "variables" |> to_assoc) with
          | _ ->
            []
        in
        let operation_name =
          Json.(payload_json |> member "operationName" |> to_string_option)
        in
        Some
          (Gql_start
             { id = opId
             ; query
             ; variables =
                 (variables :> (string * Graphql_parser.const_value) list)
             ; operation_name
             })
      | "stop" ->
        Some Gql_stop
      | "connection_terminate" ->
        Some Gql_connection_terminate
      | _ | (exception _) ->
        None
  end

  module Server = struct
    type t =
      | Gql_connection_error
      | Gql_connection_ack
      | Gql_data
      | Gql_error
      | Gql_complete

    (* | Gql_connection_keep_alive *)

    let to_string = function
      | Gql_connection_error ->
        "connection_error"
      | Gql_connection_ack ->
        "connection_ack"
      | Gql_data ->
        "data"
      | Gql_error ->
        "error"
      | Gql_complete ->
        "complete"

    (* | Gql_connection_keep_alive -> "ka" *)
  end
end

module Make (SubscriptionsManager : SubscriptionsManager) = struct
  open Websocketaf
  module Json = Yojson.Basic.Util

  type subscriptions_manager = SubscriptionsManager.t

  let create_message wsd ?(opcode = `Text) ?id ?(payload = `Null) typ =
    let open Protocol in
    let frame_payload =
      `Assoc
        [ "type", `String (Server.to_string typ)
        ; ("id", match id with Some id -> `String id | None -> `Null)
        ; "payload", payload
        ]
    in
    let json = Yojson.Basic.to_string frame_payload in
    Websocketaf.Wsd.send_bytes
      wsd
      ~kind:opcode
      ~off:0
      ~len:(String.length json)
      (Bytes.unsafe_of_string json)

  type 'a handlers =
    { schedule :
        'a
        -> on_recv:((Yojson.Basic.t, Yojson.Basic.t) result -> unit)
        -> on_close:(unit -> unit)
        -> unit
    ; destroy : 'a -> unit
    }

  let on_recv
      :  subscriptions_manager -> ?keepalive:bool
      -> subscribe:
           (variables:Graphql.Schema.variables
            -> ?operation_name:string
            -> string
            -> (( [< `Response of Yojson.Basic.t | `Stream of 'a ]
                , Yojson.Basic.t )
                result
                -> unit)
            -> unit)
      -> 'a handlers -> Websocketaf.Wsd.t
      -> opcode:Websocketaf.Websocket.Opcode.t -> is_fin:bool -> Bigstringaf.t
      -> off:int -> len:int -> unit
    =
   fun mgr ?keepalive:_keepalive ~subscribe handlers ->
    let open Protocol in
    let websocket_handler wsd ~opcode ~is_fin:_ bs ~off ~len =
      match opcode with
      | `Binary | `Continuation | `Text ->
        let json =
          Yojson.Basic.from_string (Bigstringaf.substring bs ~off ~len)
        in
        (match Client.of_json json with
        | None ->
          let id = Json.(json |> member "id" |> to_string) in
          let payload = `Assoc [ "message", `String "Invalid message type!" ] in
          create_message wsd ~id ~payload Server.Gql_error
        | Some message ->
          (match message with
          | Gql_connection_init ->
            (* TODO: allow a user-defined `on_connect` handler *)
            (* TODO: check for `graphql-ws` in the request headers, otherwise
               terminate connection *)
            create_message wsd Gql_connection_ack
          | Gql_start { id; query; variables; operation_name } ->
            (* TODO: unsubscribe if there's a subscription with the same id *)
            subscribe ~variables ?operation_name query (function
                | Error message ->
                  let payload = `Assoc [ "message", message ] in
                  create_message wsd ~payload ~id Gql_error
                | Ok (`Response json) ->
                  create_message wsd ~id ~payload:json Gql_data
                | Ok (`Stream stream) ->
                  SubscriptionsManager.add mgr id (fun () ->
                      handlers.destroy stream);
                  handlers.schedule
                    stream
                    ~on_recv:(fun x ->
                      let (Ok x | Error x) = x in
                      (* XXX: OGS doesn't yet have a way of effectively killing
                       * a stream – so if we've been asked to unsubscribe, don't
                       * push the execution result to the websocket.
                       *)
                      if SubscriptionsManager.mem mgr id then
                        create_message wsd ~id ~payload:x Gql_data)
                    ~on_close:(fun () ->
                      if SubscriptionsManager.mem mgr id then
                        create_message wsd ~id Gql_complete))
          | Gql_stop ->
            let opId = Json.(json |> member "id" |> to_string) in
            (match SubscriptionsManager.find_opt mgr opId with
            | None ->
              ()
            | Some unsubscribe ->
              unsubscribe ());
            SubscriptionsManager.remove mgr opId
          | Gql_connection_terminate ->
            SubscriptionsManager.iter (fun _ f -> f ()) mgr;
            SubscriptionsManager.clear mgr;
            Wsd.close wsd))
      | `Connection_close ->
        Websocketaf.Wsd.close wsd
      | `Ping ->
        Websocketaf.Wsd.send_pong wsd
      | `Pong | `Other _ ->
        ()
    in
    websocket_handler
end
