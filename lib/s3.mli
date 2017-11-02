(** S3 commands *)


(** S3 command type.
    If [retries] is set, then the system will retry if the command is
    throttled  using exponential backoff.

    [credentials] are credentials used for accessing s3. If none is
    supplied the command will be attempted without.

    [region] specified on which region the operation should go to. default [us-east]
*)
type 'a command = ?retries:int -> ?credentials:Credentials.t -> ?region:Util.region -> 'a


open Async
open Core

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

(** Upload [key] to [bucket].

    If gzip is true, the contents will be gzipped before uploading and
    content-encoding set to gzip so client will automatically
    decompress the content.
*)
val put :
  (?content_type:string ->
  ?gzip:bool ->
  ?acl:string ->
  ?cache_control:string ->
  bucket:string ->
  key:string ->
  string -> unit Deferred.Or_error.t) command

(** Download [key] from s3 in [bucket] *)
val get :
  (bucket:string -> key:string -> unit ->
   string Deferred.Or_error.t) command

(** Delete [key] from [bucket]. *)
val delete :
  (bucket:string -> key:string -> unit ->
   unit Deferred.Or_error.t) command

(** Delete multiple objects from [bucket].

    The result will indicate which items failed and which are deleted. If
    an item is not found it will be reported as successfully deleted.
*)
val delete_multi :
  (bucket:string -> Delete_multi.objekt list -> unit ->
   Delete_multi.result Deferred.Or_error.t) command

(** List contents in [bucket]

    Aws will return upto 1000 keys per request. If not all keys are
    returned, the function will return a continuation.
*)
val ls :
  (?continuation_token:string -> ?prefix:string -> bucket:string -> unit ->
   Ls.t) command
