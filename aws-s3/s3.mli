(** S3 commands *)

(** S3 command type.
    If [retries] is set, then the system will retry if the command is
    throttled  using exponential backoff.

    [credentials] are credentials used for accessing s3. If none is
    supplied the command will be attempted without.

    [region] specified on which region the operation should go to. default [us-east]
*)

module Make(Compat : Types.Compat) : sig
  open Compat
  open Core

  type error =
    | Redirect of Util.region
    | Throttled
    | Unknown of int * string

  type nonrec 'a result = ('a, error) result Deferred.t
  type 'a command = ?credentials:Credentials.t -> ?region:Util.region -> 'a

  module Ls : sig
    type storage_class = Standard | Standard_ia | Reduced_redundancy | Glacier
    type content = {
      storage_class : storage_class;
      size : int;
      last_modified : Time.t;
      key : string;
      etag : Caml.Digest.t;
    }
    type t = (content list * cont) result
    and cont = More of (unit -> t) | Done
  end

  module Delete_multi : sig
    type objekt = { key : string; version_id : string option; }
    type error = {
      key : string;
      version_id : string option;
      code : string;
      message : string;
    }
    type result = {
      delete_marker : bool;
      delete_marker_version_id : string option;
      deleted : objekt list;
      error : error list;
    }
  end

  type range = { first: int option; last:int option }

  (** Upload [key] to [bucket].

      If gzip is true, the contents will be gzipped before uploading and
      content-encoding set to gzip so client will automatically
      decompress the content.

      Returns the etag of the object. The etag is the md5 checksum (RFC 1864)
  *)
  val put :
    (?content_type:string ->
     ?gzip:bool ->
     ?acl:string ->
     ?cache_control:string ->
     bucket:string ->
     key:string ->
     string -> Caml.Digest.t result) command


  (** Download [key] from s3 in [bucket]
      If [range] is specified, only a part of the file is downloaded.
      - If [first] is None, then start from the beginning of the object.
      - If [last] is None, then get to the end of the object.
  *)
  val get :
    (?range:range -> bucket:string -> key:string -> unit -> string result) command

  (** Delete [key] from [bucket]. *)
  val delete :
    (bucket:string -> key:string -> unit -> unit result) command

  (** Delete multiple objects from [bucket].

      The result will indicate which items failed and which are deleted. If
      an item is not found it will be reported as successfully deleted.
  *)
  val delete_multi :
    (bucket:string -> Delete_multi.objekt list -> unit -> Delete_multi.result result) command

  (** List contents in [bucket]

      Aws will return upto 1000 keys per request. If not all keys are
      returned, the function will return a continuation.
  *)
  val ls :
    (?continuation_token:string -> ?prefix:string -> bucket:string -> unit -> Ls.t) command
end

module Test : sig
  open OUnit2
  val unit_test : test
end
