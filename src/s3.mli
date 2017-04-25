open Async.Std
open Core.Std

module Ls : sig
  type storage_class = Standard | Standard_ia | Reduced_redundancy | Glacier
  type contents = {
    storage_class : storage_class;
    size : int;
    last_modified : Time.t;
    key : string;
    etag : string;
  }

  type result = (contents list * cont) Deferred.Or_error.t
  and cont = More of (unit -> result) | Done

end

val put :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  ?content_type:string ->
  ?gzip:bool ->
  ?acl:string ->
  ?cache_control:string ->
  path:string -> String.t -> unit Deferred.Or_error.t

val get :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  path:string -> unit -> string Deferred.Or_error.t

val delete :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  path:string -> unit -> unit Deferred.Or_error.t

val delete_multi :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  bucket:string ->
  string List.t -> unit -> unit Deferred.Or_error.t

val ls :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  ?continuation_token:string -> path:string -> unit -> Ls.result
