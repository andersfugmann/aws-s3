(**/**)
include Aws_s3.Types.Compat
  with type 'a Deferred.t = 'a Async_kernel.Deferred.t
   and type 'a Pipe.reader = 'a Async_kernel.Pipe.Reader.t
(**/**)
