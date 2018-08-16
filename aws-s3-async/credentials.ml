(** Async aware Credentials.
    For API documentation
    @see <../../../aws-s3/Aws_s3/Credentials/Make/index.html>({!module:Aws_s3.Credentials.Make})
*)
include Aws_s3.Credentials.Make(Io)
type t = Aws_s3.Credentials.t
