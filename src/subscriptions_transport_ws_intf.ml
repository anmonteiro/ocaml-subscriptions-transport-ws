module type Intf = sig
  type +'a io
  type 'a stream
  type subscriptions_manager

  val on_recv : subscriptions_manager ->
    ?keepalive:int ->
    subscribe:(variables:Graphql.Schema.variables ->
               ?operation_name:string ->
               string ->
               ([< `Response of Yojson.Basic.json
                | `Stream of (Yojson.Basic.json, Yojson.Basic.json) result stream ],
                Yojson.Basic.json) result io) ->
    push_to_websocket:(Websocket.Frame.t option -> unit) Lazy.t ->
    Websocket.Frame.t ->
    unit
end
