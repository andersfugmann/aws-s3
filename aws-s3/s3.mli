(** S3 functions
    All function requires a [region], [scheme] and [credentials].

    The default region is [Us_east_1].

    The default scheme is [http]. If you are connecting from outside AWS,
    it is strongly recommended that you use https.
    To use https, make sure to have the relevant opam packages installed:
    [async_ssl] for [async] and [lwt_ssl]/[tls] for [lwt].
    Please note that connections are not reused due to a limitation on the AWS endpoint.


    If no credentials is provided, the requests will not be signed,
    The bucket / objects need to be configured accordingly.

    IPv6 connection can be set globally using the function:
    {!Aws_s3.S3.Make.set_connection_type}.
*)
module Make(Io : Types.Io) : sig
  open Io

  type error =
    | Redirect of Region.t
    | Throttled
    | Unknown of int * string
    | Failed of exn
    | Not_found

  type storage_class = Standard | Standard_ia | Onezone_ia | Reduced_redundancy | Glacier
  type content = {
    storage_class : storage_class;
    size : int;
    last_modified : float; (** Seconds since epoch *)
    key : string;
    etag : string; (** Etag as a string. this us usually the MD5, unless the object was constructed by multi-upload *)
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

  (** Globally switch between IPv4 or IPv6 connections.
      This will affect all following operations on buckets/objects.
      Defaut is to use IPv4, but may change in the future.
      Unix domain sockets are not supported.
  *)
  val set_connection_type: Unix.socket_domain -> unit

  (** Upload [data] to [bucket]/[key].
      Returns the etag of the object. The etag is the md5 checksum (RFC 1864)
      @param expect If true, the body will not be sent untill a
      status has been received from the server. This incurs a delay
      in transfer, but avoid sending a large body, if the request is
      know to fail before the body is sent.
  *)
  val put :
    (?content_type:string ->
     ?content_encoding:string ->
     ?acl:string ->
     ?cache_control:string ->
     ?expect:bool ->
     bucket:string ->
     key:string ->
     data:string -> unit -> string result) command

  (** Download [key] from s3 in [bucket]
      If [range] is specified, only a part of the file is retrieved:
      - If [first] is None, then start from the beginning of the object.
      - If [last] is None, then get to the end of the object.
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
      an item is not found it will be reported as successfully deleted
      (the operation is idempotent).
  *)
  val delete_multi :
    (bucket:string -> objects:Delete_multi.objekt list -> unit -> Delete_multi.result result) command

  (** List contents in [bucket]
      Aws will return at most 1000 keys per request. If not all keys are
      returned, the function will return a continuation.
  *)
  val ls :
    (?continuation_token:string -> ?prefix:string -> ?max_keys:int -> bucket:string -> unit -> Ls.t) command

  (** Streaming functions.
      Streaming function seeks to limit the amount of used memory used when
      operating of large objects by operating on streams.
  *)
  module Stream : sig

    (** Streaming version of put.
        @param length Amount of data to copy
        @param chunk_size The size of chunks send to s3.
               The system will have 2 x chunk_size byte in flight

        see {!Aws_s3.S3.Make.put}
    *)
    val put :
      (?content_type:string ->
       ?content_encoding:string ->
       ?acl:string ->
       ?cache_control:string ->
       ?expect:bool ->
       bucket:string ->
       key:string ->
       data:string Io.Pipe.reader ->
       chunk_size:int ->
       length:int ->
       unit -> string result) command

    (** Streaming version of get.
        The caller must supply a [sink] to which retrieved data is streamed.
        The result will be determined after all data has been sent to the sink, and the sink is closed.

        Connections to s3 is closed once the result has been determined.
        The caller should ways examine the result of the function.
        If the result is [Ok], then it is guaranteed that all data has been retrieved successfully and written to the sink.
        In case of [Error], only parts of the data may have been written to the sink.

        The rationale for using a sink rather than returning a pipe reader from which data
        can be consumed is that a reader does not allow simple relay of error states during the transfer.

        For other parameters see {!Aws_s3.S3.Make.get}
    *)
    val get :
      (?range:range -> bucket:string -> key:string -> sink:string Io.Pipe.writer -> unit -> unit result) command

  end

  module Multipart_upload: sig
    type t

    (** Initialize multipart upload *)
    val init :
      (?content_type:string ->
      ?content_encoding:string * string ->
      ?acl:string ->
      ?cache_control:string ->
      bucket:string -> key:string -> unit -> t result) command

    (** Upload a part of the file. All parts except the last part must
       be at least 5Mb big. All parts must have a unique part number.
       The final file will be assembled from all parts ordered by part
       number

       @param expect: If true, the body will not be sent until a
       tatus has been received from the server. This incurs a delay
       in transfer, but avoid sending a large body, if the request is
       know to fail before the body is sent.
    *)
    val upload_part :
      (t ->
       part_number:int ->
       ?expect:bool ->
       data:string ->
       unit ->
       unit result) command

    (** Specify a part as a copy of an existing object in S3. *)
    val copy_part :
      (t -> part_number:int -> ?range:int * int -> bucket:string -> key:string -> unit -> unit result) command

    (** Complete a multipart upload. The returned string is an opaque identifier used as etag *)
    val complete : (t -> unit -> string result) command

    (** Abort a multipart upload. This also discards all uploaded parts. *)
    val abort : (t -> unit -> unit result) command

    (** Streaming functions *)
    module Stream : sig

      (** Streaming version of upload_part.
          @param length is the amount of data to copy
          @param chunk_size Is the size of chunks send to s3.
                 The system will have 2 x chunk_size byte in flight


          see {!Aws_s3.S3.Make.Multipart_upload.upload_part}
      *)
      val upload_part :
        (t ->
         part_number:int ->
         ?expect:bool ->
         data:string Io.Pipe.reader ->
         length:int ->
         chunk_size:int ->
         unit ->
         unit result) command
    end
  end

  (** Helper function to handle error codes.
      The function handle redirects and throttling.
  *)
  val retry : ?region:Region.t -> retries:int ->
    f:(?region:Region.t -> unit -> 'a result) -> unit -> 'a result
end
