(** Parse command line options *)
open Cmdliner

type actions =
  | Ls of { bucket: string; prefix: string option; start_after: string option; ratelimit: int option; max_keys: int option}
  | Head of { path: string; }
  | Rm of { bucket: string; paths : string list }
  | Cp of { src: string; dest: string; first: int option; last: int option; multi: bool; chunk_size: int option}

type options =
  { profile: string option; minio: string option; https: bool; retries: int; ipv6: bool; expect: bool; confirm_requester_pays : bool }

let parse exec =
  let profile =
    let doc = "Specify profile to use." in
    Arg.(value & opt (some string) None & info ["profile"; "p"] ~docv:"PROFILE" ~doc)
  in

  let ratelimit =
    let doc = "Limit requests to N/sec." in
    Arg.(value & opt (some int) None & info ["ratelimit"; "r"] ~docv:"LIMIT" ~doc)
  in

  let https =
    let doc = "Enable/disable https." in
    Arg.(value & opt bool false & info ["https"] ~docv:"HTTPS" ~doc)
  in

  let minio =
    let doc = "Connect to minio address <host>[:port]" in
    Arg.(value & opt (some string) None & info ["minio"] ~docv:"MINIO" ~doc)
  in

  let ipv6 =
    let doc = "Use ipv6" in
    Arg.(value & flag & info ["6"] ~docv:"IPV6" ~doc)
  in

  let retries =
    let doc = "Retries in case of error" in
    Arg.(value & opt int 0 & info ["retries"] ~docv:"RETRIES" ~doc)
  in

  let expect =
    let doc = "Use expect -> 100-continue for put/upload_chunk" in
    Arg.(value & flag & info ["expect"; "e"] ~docv:"EXPECT" ~doc)
  in

  let confirm_requester_pays =
    let doc = "indicate that the client is willing to pay for the \
               request, should the target bucket be configured to \
               impose those costs on the requester." in
    Arg.(value & flag & info ["requester-pays"] ~docv:"REQUESTER-PAYS" ~doc)
  in

  let common_opts =
    let make profile minio https retries ipv6 expect confirm_requester_pays = { profile; minio; https; retries; ipv6; expect; confirm_requester_pays } in
    Term.(const make $ profile $ minio $ https $ retries $ ipv6 $ expect $ confirm_requester_pays )
  in

  let bucket n =
    let doc = "S3 bucket name" in
    Arg.(required & pos n (some string) None & info [] ~docv:"BUCKET" ~doc)
  in

  let path n name =
    let doc = "path: <local_path>|s3://<bucket>/<objname>" in
    Arg.(required & pos n (some string) None & info [] ~docv:name ~doc)
  in

  let cp =
    let make opts first last multi chunk_size src dest =
      opts, Cp { src; dest; first; last; multi; chunk_size }
    in
    let first =
      let doc = "first byte of the source object to copy. If omitted means from the start." in
      Arg.(value & opt (some int) None & info ["first"; "f"] ~docv:"FIRST" ~doc)
    in
    let last =
      let doc = "last byte of the source object to copy. If omitted means to the end" in
      Arg.(value & opt (some int) None & info ["last"; "l"] ~docv:"LAST" ~doc)
    in
    let multi =
      let doc = "Use multipart upload" in
      Arg.(value & flag & info ["multi"; "m"] ~docv:"MULTI" ~doc)
    in

    let chunk_size =
      let doc = "Use streaming get / put the given chunk_size" in
      Arg.(value & opt (some int) None & info ["chunk-size"; "c"] ~docv:"CHUNK SIZE" ~doc)
    in
    Cmd.v
      Cmd.(info "cp" ~doc:"Copy files to and from S3")
      Term.(const make $ common_opts $ first $ last $ multi $ chunk_size $ path 0 "SRC" $ path 1 "DEST")
  in
  let rm =
    let objects =
      let doc = "name of the object to delete" in
      Arg.(non_empty & pos_right 0 string [] & info [] ~docv:"OBJECT" ~doc)
    in

    let make opts bucket paths = opts, Rm { bucket; paths } in
    Cmd.v
      Cmd.(info "rm" ~doc:"Delete files from s3")
      Term.(const make $ common_opts $ bucket 0 $ objects)
  in

  let head =
    let path =
      let doc = "object: s3://<bucket>/<objname>" in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)
    in

    let make opts path = opts, Head { path } in
    Cmd.v
      Cmd.(info "head" ~doc:"Head files from s3")
      Term.(const make $ common_opts $ path)
  in

  let ls =
    let make opts ratelimit prefix start_after bucket max_keys = opts, Ls { bucket; prefix; start_after; ratelimit; max_keys } in

    let prefix =
      let doc = "Only list elements with the given prefix" in
      Arg.(value & opt (some string) None & info ["prefix"] ~docv:"PREFIX" ~doc)
    in

    let max_keys =
      let doc = "Max keys returned per ls request" in
      Arg.(value & opt (some int) None & info ["max-keys"] ~docv:"MAX KEYS" ~doc)
    in

    let start_after =
      let doc = "List objects after the given key" in
      Arg.(value & opt (some string) None & info ["start-after"] ~docv:"START AFTER" ~doc)
    in

    Cmd.v
    (Cmd.info "ls" ~doc:"List files in bucket")
    (Term.(const make $ common_opts $ ratelimit $ prefix $ start_after $ bucket 0 $ max_keys))
  in

  (* Where do the result go? *)
  let help =
    let doc = "Amazon s3 command line interface" in
    Cmd.info Sys.argv.(0) ~doc
  in

  let commands =
    let cmds = [cp; rm; ls; head] in
    Cmd.group (help) cmds
    |> Cmd.eval_value
  in
  let run = function
    | Ok (`Ok cmd) -> exec cmd
    | _ -> 254
  in
  run @@ commands |> exit
