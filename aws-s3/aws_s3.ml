module S3 = S3
module Types = Types
module Credentials = Credentials
module Region = Region
module Authorization = Authorization
module Auth : sig val make_presigned_url :
  ?scheme:string ->
  credentials:Credentials.t ->
  date:Ptime.t ->
  region:Region.t->
  path:string ->
  bucket:string ->
  verb:string ->
  duration:int ->
  unit ->
  Uri.t
  end = Authorization
