(** Simple unit tests *)
open Aws_s3
open OUnit2
open Async

let async_runner f ctx =
  Thread_safe.block_on_async_exn (fun () -> f ctx)


let suite =
  "aws-s3" >::: [
    S3.Test.unit_test async_runner;
    Util.Test.unit_test async_runner;
  ]

let () = run_test_tt_main suite
