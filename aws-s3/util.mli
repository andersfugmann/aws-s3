(** Utilites *)

open Core
open Cohttp

type region =
 | Ap_northeast_1  (* Asia Pacific (Tokyo) *)
 | Ap_northeast_2  (* Asia Pacific (Seoul) *)
 | Ap_northeast_3  (* Asia Pacific (Osaka-Local) *)
 | Ap_southeast_1  (* Asia Pacific (Singapore) *)
 | Ap_southeast_2  (* Asia Pacific (Sydney) *)
 | Ap_south_1      (* Asia Pacific (Mumbai) *)
 | Eu_central_1    (* EU (Frankfurt) *)
 | Cn_northwest_1  (* China (Ningxia)        *)
 | Cn_north_1      (* China (Beijing)        *)
 | Eu_west_1       (* EU (Ireland)   *)
 | Eu_west_2       (* EU (London)    *)
 | Eu_west_3       (* EU (Paris)     *)
 | Sa_east_1       (* South America (São Paulo)      *)
 | Us_east_1       (* US East (N. Virginia) *)
 | Us_east_2       (* US East (Ohio) *)
 | Us_west_1       (* US West (N. California) *)
 | Us_west_2       (* US West (Oregon) *)
 | Ca_central_1    (* Canada - central *)
 | Other of string (* Other unknown *)

val host_of_region : region -> string
val string_of_region : region -> string

val region_of_string : string -> region
val region_of_host : string -> region

module Make : functor(Compat: Types.Compat) -> sig
  open Compat

  val make_request :
    scheme:[`Http|`Https] ->
    ?body:String.t ->
    ?region:region ->
    ?credentials:Credentials.t ->
    headers:(string * string) list ->
    meth:[`GET | `PUT | `POST | `DELETE | `HEAD ] ->
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
