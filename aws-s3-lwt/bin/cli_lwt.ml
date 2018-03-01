module Aws = Aws_cli.Aws.Make(Aws_s3_lwt.Compat)

let exec = function
  | `Ok a ->
    Lwt_main.run (Aws.exec a)
  | _ -> ()

let () =
  Aws_cli.Cli.parse exec
