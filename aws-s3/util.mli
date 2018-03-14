(** Utilites *)

open Core
open Cohttp

type region =
  | Ap_northeast_1
  | Ap_southeast_1
  | Ap_southeast_2
  | Eu_central_1
  | Eu_west_1
  | Sa_east_1
  | Us_east_1
  | Us_west_1
  | Us_west_2

val region_of_host : string -> region
val region_of_string : string -> region
val gzip_data : ?level:int -> string -> string

module Make : functor(Compat: Types.Compat) -> sig
  open Compat

  val make_request :
    ?body:String.t ->
    ?region:region ->
    ?credentials:Credentials.t ->
    headers:(string * string) list ->
    meth:Code.meth ->
    path:string ->
    query:(string * string) list ->
    unit ->
    (Response.t * Cohttp_deferred.Body.t) Deferred.t

end

(*
module Test : sig
  open OUnit2
  val unit_test : ((test_ctxt -> unit Deferred.t) -> test_ctxt -> unit) -> test
end
*)
