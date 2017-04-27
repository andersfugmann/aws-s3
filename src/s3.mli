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

  type t = (contents list * cont) Deferred.Or_error.t
  and cont = More of (unit -> t) | Done
end

module Delete_multi : sig
  type objekt = { key : string; version_id : string option; }

  type deleted = { key : string; version_id : string option; }
  type error = {
    key : string;
    version_id : string option;
    code : string;
    message : string;
  }
  type result = {
    delete_marker : bool;
    delete_marker_version_id : string option;
    deleted : deleted list;
    error : error list;
  }
end

val put :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  ?content_type:string ->
  ?gzip:bool ->
  ?acl:string ->
  ?cache_control:string ->
  bucket:string ->
  key:string ->
  string -> unit Deferred.Or_error.t

val get :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  bucket:string ->
  key:string ->
  unit -> string Deferred.Or_error.t

val delete :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  bucket:string ->
  key:string ->
  unit -> unit Deferred.Or_error.t

val delete_multi :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  bucket:string ->
  Delete_multi.objekt list -> unit -> Delete_multi.result Deferred.Or_error.t

val ls :
  ?retries:int ->
  ?credentials:Credentials.t ->
  ?region:Util.region ->
  ?continuation_token:string -> ?prefix:string -> bucket:string -> unit -> Ls.t
