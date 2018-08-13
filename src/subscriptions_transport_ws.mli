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

module Make
    (Io : IO)
    (Stream : Stream with type 'a io = 'a Io.t)
    (SubscriptionsManager : SubscriptionsManager) : sig
  val on_recv : SubscriptionsManager.t ->
    ?keepalive:int ->
    subscribe:(variables:Graphql.Schema.variables ->
               ?operation_name:string ->
               string ->
               ([< `Response of Yojson.Basic.json
                | `Stream of (Yojson.Basic.json, Yojson.Basic.json) result Stream.t ],
                Yojson.Basic.json) result Io.t) ->
    push_to_websocket:(Websocket.Frame.t option -> unit) Lazy.t ->
    Websocket.Frame.t ->
    unit
end
