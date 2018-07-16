module HeaderMap : Map.S with type key = string
val hash_sha256 : string -> Digestif.SHA256.t
val hmac_sha256 : key:string -> string -> Digestif.SHA256.t
val to_hex : Digestif.SHA256.t -> string

val make_signing_key :
  date:string ->
  region:string ->
  credentials:Credentials.t ->
  service:string -> Digestif.SHA256.t

val string_to_sign :
  date:string ->
  time:string ->
  verb:string ->
  path:string ->
  query:string ->
  headers:string HeaderMap.t ->
  payload_sha:string -> region:string -> service:string -> string * string

val make_authorization :
  date:string ->
  time:string ->
  verb:string ->
  credentials:Credentials.t ->
  path:string ->
  headers:string HeaderMap.t ->
  query:string ->
  region:string -> service:string -> payload_sha:string -> string
