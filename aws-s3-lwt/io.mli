(**/**)
include Aws_s3.Types.Io
  with type 'a Deferred.t = 'a Lwt.t
   and type 'a Pipe.reader = 'a Lwt_stream.t
(**/**)
