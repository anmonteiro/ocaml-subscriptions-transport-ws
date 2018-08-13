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
    (SubscriptionsManager : SubscriptionsManager) :
  Subscriptions_transport_ws_intf.Intf with type 'a io = 'a Io.t
                                        and type 'a stream = 'a Stream.t
                                        and type subscriptions_manager = SubscriptionsManager.t
