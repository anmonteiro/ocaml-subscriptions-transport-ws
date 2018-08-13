module Make (SubscriptionsManager : Subscriptions_transport_ws.SubscriptionsManager) :
  Subscriptions_transport_ws_intf.Intf with type +'a io = 'a Async_kernel.Deferred.t
                                        and type 'a stream = 'a Async_kernel.Pipe.Reader.t
                                        and type subscriptions_manager = SubscriptionsManager.t
