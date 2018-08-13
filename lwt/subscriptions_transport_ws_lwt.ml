module Stream = struct
  type +'a io = 'a Lwt.t
  type 'a t = 'a Lwt_stream.t * (unit -> unit)

  let rec consume_stream (stream, _ as s) cb =
    let open Lwt.Infix in
    Lwt.catch
      (fun () ->
         Lwt_stream.next stream >>= fun x ->
         cb x;
         consume_stream s cb)
      (fun _ -> Printf.eprintf "AMIGOS\n%!";Lwt.return_unit)

  let stream_destroy_fn (_, destroy) = destroy
end



module Make (SubscriptionsManager : Subscriptions_transport_ws.SubscriptionsManager) =
  Subscriptions_transport_ws.Make (Lwt) (Stream)

