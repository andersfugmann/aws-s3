(** Utilites *)

(**/**)
module R = Result
(**/**)

open Core
open Async
open Cohttp
open Cohttp_async

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

val region_of_string : string -> region

(**/**)
val gzip_data : ?level:int -> String.t -> string
val make_request :
  ?body:String.t ->
  ?region:region ->
  ?credentials:Credentials.t ->
  headers:(string * string) list ->
  meth:Code.meth ->
  path:string ->
  query:(string * string) list ->
  unit ->
  (Response.t * Body.t) Deferred.t


module Test : sig
  open OUnit2
  val unit_test : ((test_ctxt -> unit Async.Deferred.t) -> test_ctxt -> unit) -> test
end
