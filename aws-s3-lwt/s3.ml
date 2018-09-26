(** Lwt aware S3 commands.
    For API documentation
    @see <../../../aws-s3/Aws_s3/S3/Make/index.html>({!module:Aws_s3.S3.Make})

    Lwt uses a default buffer size of 4096 bytes, which means that on high speed connections
    lwt will make needlessly many read / write system calls.

    When transferring data over high speed connections, it is recommended to increase
    the global channel buffer size e.g: Lwt_io.set_default_buffer_size (128*1024).

    Each operation will allocate at least 2*default_buffer_size.
*)
include Aws_s3.S3.Make(Io)
