(** S3 commands *)
module Make(Compat : Types.Compat) : sig
  open Compat
  open Core

  type error =
    | Redirect of Region.t
    | Throttled
    | Unknown of int * string
    | Not_found

  type storage_class = Standard | Standard_ia | Onezone_ia | Reduced_redundancy | Glacier
  type content = {
    storage_class : storage_class;
    size : int;
    last_modified : Time.t;
    key : string;
    etag : string;
  }

  type nonrec 'a result = ('a, error) result Deferred.t
  type 'a command = ?scheme:[`Http|`Https] -> ?credentials:Credentials.t -> ?region:Region.t -> 'a

  module Ls : sig
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
      Returns the etag of the object. The etag is the md5 checksum (RFC 1864)
  *)
  val put :
    (?content_type:string ->
     ?content_encoding:string ->
     ?acl:string ->
     ?cache_control:string ->
     bucket:string ->
     key:string ->
     data:string -> unit -> string result) command


  (** Download [key] from s3 in [bucket]
      If [range] is specified, only a part of the file is retrieved.
      - If [first] is None, then start from the beginning of the object.
      - If [last] is None, then get to the end of the object.
      Scheme defaults to http. If you are uploading across the internet.
      to use https, please make sure that you have enabled ssl for cohttp
      (opam package tls or lwt_ssl for lwt or async_ssl for async)
  *)
  val get :
    (?range:range -> bucket:string -> key:string -> unit -> string result) command

  (** Call head on the object to retrieve info on a single object *)
  val head :
    (bucket:string -> key:string -> unit -> content result) command

  (** Delete [key] from [bucket]. *)
  val delete :
    (bucket:string -> key:string -> unit -> unit result) command

  (** Delete multiple objects from [bucket].

      The result will indicate which items failed and which are deleted. If
      an item is not found it will be reported as successfully deleted.
  *)
  val delete_multi :
    (bucket:string -> objects:Delete_multi.objekt list -> unit -> Delete_multi.result result) command

  (** List contents in [bucket]

      Aws will return upto 1000 keys per request. If not all keys are
      returned, the function will return a continuation.
  *)
  val ls :
    (?continuation_token:string -> ?prefix:string -> bucket:string -> unit -> Ls.t) command

  module Multipart_upload: sig
    type t

    (** Initialize multipart upload *)
    val init :
      (?content_type:string ->
      ?content_encoding:string * string ->
      ?acl:string ->
      ?cache_control:string ->
      bucket:string -> key:string -> unit -> t result) command

    (** Upload a part of the file. All parts except the last part must be
        at least 5Mb big. All parts must have a unique part number.
        The final file will be assembled from all parts ordered by part number *)
    val upload_part :
      (t -> part_number:int -> data:string -> unit -> unit result) command

    (** Specify a part as a copy of an existing object in S3. *)
    val copy_part :
      (t -> part_number:int -> ?range:int * int -> bucket:string -> key:string -> unit -> unit result) command

    (** Complete a multipart upload. The returned string is an opaque identifier used as etag *)
    val complete : (t -> unit -> string result) command

    (** Abort a multipart upload. This also discards all uploaded parts. *)
    val abort : (t -> unit -> unit result) command
  end

  (** Helper function to handle error codes.
      The function handle redirects and throttling.
  *)
  val retry : ?region:Region.t -> retries:int ->
    f:(?region:Region.t -> unit -> ('a, error) Result.t Deferred.t) -> unit ->
    ('a, error) Result.t Deferred.t
end

module Test : sig
  open OUnit2
  val unit_test : test
end
