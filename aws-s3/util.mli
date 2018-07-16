(** Utilites *)
open Cohttp

module Make : functor(Compat: Types.Compat) -> sig
  open Compat

  val make_request :
    scheme:[`Http|`Https] ->
    ?body:String.t ->
    ?region:Region.t ->
    ?credentials:Credentials.t ->
    headers:(string * string) list ->
    meth:[`GET | `PUT | `POST | `DELETE | `HEAD ] ->
    path:string ->
    query:(string * string) list ->
    unit ->
    (Response.t * Cohttp_deferred.Body.t) Deferred.t

end
