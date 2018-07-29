(** Utilites *)

module Make : functor(Io: Types.Io) -> sig
  open Io

  val string_of_method : [< `DELETE | `GET | `HEAD | `POST | `PUT ] -> string

  val call:
    ?domain:Unix.socket_domain ->
    ?expect:bool ->
    scheme:[< `Http | `Https ] ->
    host:string ->
    path:string ->
    ?query:(string * string) list ->
    headers:string Headers.t ->
    ?body:string Pipe.reader ->
    [< `DELETE | `GET | `HEAD | `POST | `PUT ] ->
    (int * string * string Headers.t * string Pipe.reader) Deferred.Or_error.t
end
