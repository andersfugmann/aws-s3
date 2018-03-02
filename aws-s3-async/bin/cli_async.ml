module Aws = Aws_cli.Aws.Make(Aws_s3_async.Compat)

let exec cmd =
  Async.Thread_safe.block_on_async_exn (fun () -> Aws.exec cmd)

let () =
  Aws_cli.Cli.parse exec
