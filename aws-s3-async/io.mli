(**/**)
include Aws_s3.Types.Io
  with type 'a Deferred.t = 'a Async_kernel.Deferred.t
   and type ('a, 'b) Pipe.pipe = ('a, 'b) Async_kernel.Pipe.t
   and type Pipe.reader_phantom = Async_kernel.Pipe.Reader.phantom
   and type Pipe.writer_phantom = Async_kernel.Pipe.Writer.phantom
   and type 'a Pipe.reader = 'a Async_kernel.Pipe.Reader.t
   and type 'a Pipe.writer = 'a Async_kernel.Pipe.Writer.t
(**/**)
