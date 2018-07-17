module HeaderMap : Map.S with type key = string
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
  query:string ->
  headers:string HeaderMap.t ->
  payload_sha:string -> scope:string ->
  string * string

val make_authorization :
  date:string ->
  time:string ->
  verb:string ->
  credentials:Credentials.t ->
  path:string ->
  headers:string HeaderMap.t ->
  query:string ->
  scope:string ->
  signing_key:Digestif.SHA256.t ->
  payload_sha:string -> string

val chunk_signature:
  signing_key:string ->
  date_time:string ->
  scope:string ->
  previous_signature:string ->
  sha:Digestif.SHA256.t -> string

val chunk_header: length:string -> signature:string -> string
