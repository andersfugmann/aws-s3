(**/**)
module Make(Io : Types.Io) : sig
  open Io
  val make_request :
    endpoint:Region.endpoint ->
    ?connect_timeout_ms:int ->
    ?expect:bool ->
    sink:string Io.Pipe.writer ->
    ?body:Body.Make(Io).t ->
    ?credentials:Credentials.t ->
    headers:(string * string) list ->
    meth:[`GET | `PUT | `POST | `DELETE | `HEAD ] ->
    path:string ->
    query:(string * string) list ->
    unit ->
    (int * string * string Headers.t * string) Deferred.Or_error.t
end
(**/**)
