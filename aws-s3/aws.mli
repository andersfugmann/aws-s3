(**/**)
module Make_http : functor(Http : Types.Http) -> sig
  open Http.Io
  val make_request :
    endpoint:Region.endpoint ->
    ?connect_timeout_ms:int ->
    ?expect:bool ->
    sink:string Http.Io.Pipe.writer ->
    ?body:Body.Make(Http.Io).t ->
    ?credentials:Credentials.t ->
    headers:(string * string) list ->
    meth:Types.meth ->
    path:string ->
    query:(string * string) list ->
    unit ->
    (int * string * string Headers.t * string) Deferred.Or_error.t
end

module Make : functor(Io : Types.Io) -> module type of Make_http(Http.Make(Io))
(**/**)
