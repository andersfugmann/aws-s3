(**/**)
val hash_sha256 : string -> Digestif.SHA256.t
val hmac_sha256 : key:string -> string -> Digestif.SHA256.t
val to_hex : Digestif.SHA256.t -> string

val make_signing_key :
  ?bypass_cache:bool ->
  date:string ->
  region:string ->
  credentials:Credentials.t ->
  service:string -> unit -> Digestif.SHA256.t

val make_scope : date:string -> region:string -> service:string -> string

val string_to_sign :
  date:string ->
  time:string ->
  verb:string ->
  path:string ->
  query:(string * string) list ->
  headers:string Headers.t ->
  payload_sha:string -> scope:string ->
  string * string

val make_signature :
  date:string ->
  time:string ->
  verb:string ->
  path:string ->
  headers:string Headers.t ->
  query:(string * string) list ->
  scope:string ->
  signing_key:Digestif.SHA256.t ->
  payload_sha:string ->
  string * string

val make_auth_header :
  credentials:Credentials.t ->
  scope:string ->
  signed_headers:string ->
  signature:string -> string

val chunk_signature:
  signing_key:Digestif.SHA256.t ->
  date:string ->
  time:string ->
  scope:string ->
  previous_signature:string ->
  sha:Digestif.SHA256.t -> Digestif.SHA256.t
(**/**)

  (** This makes a presigned url that can be used to upload or download a file from s3 without any credentials other than those embedded in the url. [verb] should be either the string GET for download or PUT for upload.*)
val make_presigned_url :
  ?scheme:[`Http | `Https] ->
  ?host:string ->
  ?port:int ->
  credentials:Credentials.t ->
  date:Ptime.t ->
  region:Region.t ->
  path:string ->
  bucket:string ->
  verb:[`Get | `Put] ->
  duration:int ->
  unit ->
  Uri.t
