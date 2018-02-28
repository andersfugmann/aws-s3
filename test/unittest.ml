(** Simple unit tests *)
open OUnit2

let suite =
  "aws-s3" >::: [
    Aws_s3.S3.Test.unit_test;
  ]

let () = run_test_tt_main suite
