type region =
    [ `Ap_northeast_1
    | `Ap_southeast_1
    | `Ap_southeast_2
    | `Eu_central_1
    | `Eu_west_1
    | `Sa_east_1
    | `Us_east_1
    | `Us_west_1
    | `Us_west_2 ]

val gzip_data : ?level:int -> Core.Std.String.t -> string

val put :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:region ->
  ?content_type:string ->
  ?gzip:bool ->
  ?acl:string ->
  ?cache_control:string ->
  path:string -> Core.Std.String.t -> unit Async.Std.Deferred.Or_error.t

val get :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:region ->
  path:string -> unit -> string Async.Std.Deferred.Or_error.t

val delete :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:region ->
  path:string -> unit -> unit Async.Std.Deferred.Or_error.t

val delete_multi :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:region ->
  bucket:string ->
  string Core.Std.List.t -> unit -> unit Async.Std.Deferred.Or_error.t

type entry = { objekt : string; size : int; }
type ls_result = (entry list * ls_cont) Async.Std.Deferred.Or_error.t
and ls_cont = More of (unit -> ls_result) | Done
val ls :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:region ->
  ?continuation_token:string -> path:string -> unit -> ls_result

module Test : sig
  val unit_test : OUnit2.test
end
