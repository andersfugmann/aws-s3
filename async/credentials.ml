module Credentials = Aws_s3.Credentials.Make(Compat_async)
include Credentials
type t = Aws_s3.Credentials.t
