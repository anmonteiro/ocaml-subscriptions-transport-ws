module Make (SubscriptionsManager : Subscriptions_transport_ws.SubscriptionsManager) :
  Subscriptions_transport_ws_intf.Intf with type +'a io = 'a Lwt.t
                                        and type 'a stream = 'a Lwt_stream.t * (unit -> unit)
                                        and type subscriptions_manager = SubscriptionsManager.t
