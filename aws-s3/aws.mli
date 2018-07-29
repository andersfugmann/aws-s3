module Make(Io : Types.Io) : sig
  open Io
  val make_request :
    ?domain:Unix.socket_domain ->
    scheme:[`Http|`Https] ->
    ?expect:bool ->
    ?body:Body.Make(Io).body ->
    ?region:Region.t ->
    ?credentials:Credentials.t ->
    headers:(string * string) list ->
    meth:[`GET | `PUT | `POST | `DELETE | `HEAD ] ->
    path:string ->
    query:(string * string) list ->
    unit ->
    (int * string * string Headers.t * string Pipe.reader) Deferred.Or_error.t
end
