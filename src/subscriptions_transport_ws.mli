module type SubscriptionsManager = sig
  type t

  val subscriptions : t -> t

  val add : t -> string -> (unit -> unit) -> unit

  val create : int -> t

  val remove : t -> string -> unit

  val mem : t -> string -> bool

  val find_opt : t -> string -> (unit -> unit) option

  val iter : (string -> (unit -> unit) -> unit) -> t -> unit

  val clear : t -> unit
end

module Make (SubscriptionsManager : SubscriptionsManager) : sig
  type subscriptions_manager

  type 'a handlers =
    { schedule :
        'a
        -> on_recv:((Yojson.Basic.t, Yojson.Basic.t) result -> unit)
        -> on_close:(unit -> unit)
        -> unit
    ; destroy : 'a -> unit
    }

  val on_recv
    :  subscriptions_manager
    -> ?keepalive:bool
    -> subscribe:
         (variables:Graphql.Schema.variables
          -> ?operation_name:string
          -> string
          -> (( [< `Response of Yojson.Basic.t | `Stream of 'a ]
              , Yojson.Basic.t )
              result
              -> unit)
          -> unit)
    -> 'a handlers
    -> Websocketaf.Wsd.t
    -> opcode:Websocketaf.Websocket.Opcode.t
    -> is_fin:bool
    -> Bigstringaf.t
    -> off:int
    -> len:int
    -> unit
end
with type subscriptions_manager = SubscriptionsManager.t
