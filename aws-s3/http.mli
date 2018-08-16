(**/**)
type meth = [ `DELETE | `GET | `HEAD | `POST | `PUT ]

module Make : functor(Io: Types.Io) -> sig
  open Io

  val string_of_method : meth -> string

  val call:
    ?domain:Unix.socket_domain ->
    ?expect:bool ->
    scheme:[< `Http | `Https ] ->
    host:string ->
    path:string ->
    ?query:(string * string) list ->
    headers:string Headers.t ->
    sink:string Io.Pipe.writer ->
    ?body:string Pipe.reader ->
    meth ->
    (int * string * string Headers.t * string) Deferred.Or_error.t
end
(**/**)
