module Io = struct
  type +'a t = 'a Async_kernel.Deferred.t

  let return_unit = Async_kernel.Deferred.unit

  let finalize f g =
    let res = f () in
    Async_kernel.upon res (fun _ -> g () |> ignore);
    res

  let (>>=) = Async_kernel.Deferred.Infix.(>>=)

end

module Stream = struct
  type +'a io = 'a Async_kernel.Deferred.t
  type 'a t = 'a Async_kernel.Pipe.Reader.t

  let rec consume_stream pipe cb =
    let open Async_kernel.Deferred.Infix in
    Async_kernel.Pipe.read pipe >>= function
    | `Ok x ->
      (* let Ok x | Error x = x in *)
      cb x;
      consume_stream pipe cb
    | `Eof -> Async_kernel.Deferred.unit

  let stream_destroy_fn reader = (fun () -> Async_kernel.Pipe.close_read reader)
end

module Make (SubscriptionsManager : Subscriptions_transport_ws.SubscriptionsManager) =
  Subscriptions_transport_ws.Make (Io) (Stream)
