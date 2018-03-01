module Aws = Aws_cli.Aws.Make(Aws_s3_async.Compat)

let exec = function
  | `Ok a ->
    Async.Thread_safe.block_on_async_exn (fun () -> Aws.exec a)
  | _ -> ()

let () =
  Aws_cli.Cli.parse exec
