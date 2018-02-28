module S3 = Aws_s3.S3.Make(Compat_async)
include S3

(* Some inner module. I'd like to rewrite the signature *)
module type X = sig
  type t
  val make: int -> t
end

module I : X = struct
  type t = int
  let make x = x
end

module type XX = X with type t = int
