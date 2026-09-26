(* S3 built over a caller-supplied http module: the functor application is the
   check, the request only proves the result travels back out. *)
module Stub_http = struct
  module Io = Aws_s3_lwt.Io

  let call ?expect:_ ?connect_timeout_ms:_ ~endpoint:_ ~path:_ ?query:_
      ~headers:_ ~sink ?body:_ (_ : Aws_s3.Types.meth) =
    Io.Pipe.close sink;
    Io.Deferred.Or_error.return (204, "No Content", Aws_s3.Headers.empty, "")
end

module S3 = Aws_s3.S3.Make_http(Stub_http)

let endpoint =
  Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Http (Aws_s3.Region.of_string "us-east-1")

let () =
  match Lwt_main.run (S3.delete ~endpoint ~bucket:"bucket" ~key:"key" ()) with
  | Ok () -> print_endline "custom http module accepted"
  | Error _ -> failwith "custom http module: delete failed"
