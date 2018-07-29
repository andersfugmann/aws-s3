(** Parse command line options *)
open Cmdliner

type actions =
  | Ls of { bucket: string; prefix: string option; ratelimit: int option; }
  | Head of { path: string; }
  | Rm of { bucket: string; paths : string list }
  | Cp of { src: string; dest: string; first: int option; last: int option; multi: bool; chunk_size: int option}

type options =
  { profile: string option; https: bool; retries: int; ipv6: bool}

let parse exec =
  let profile =
    let doc = "Specify profile to use." in
    Arg.(value & opt (some string) None & info ["profile"; "p"] ~docv:"NAME" ~doc)
  in

  let ratelimit =
    let doc = "Limit requests to N/sec." in
    Arg.(value & opt (some int) None & info ["ratelimit"; "r"] ~docv:"INT" ~doc)
  in

  let https =
    let doc = "Enable/disable https." in
    Arg.(value & opt bool false & info ["https"] ~docv:"BOOL" ~doc)
  in

  let ipv6 =
    let doc = "Use ipv6" in
    Arg.(value & flag & info ["6"] ~docv:"IPV6" ~doc)
  in

  let retries =
    let doc = "Retries in case of error" in
    Arg.(value & opt int 0 & info ["retries"] ~docv:"INT" ~doc)
  in

  let common_opts =
    let make profile https retries ipv6 = { profile; https; retries; ipv6:bool } in
    Term.(const make $ profile $ https $ retries $ ipv6)
  in

  let bucket n =
    let doc = "S3 bucket name" in
    Arg.(required & pos n (some string) None & info [] ~docv:"NAME" ~doc)
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
      Arg.(value & opt (some int) None & info ["first"; "f"] ~docv:"INT" ~doc)
    in
    let last =
      let doc = "last byte of the source object to copy. If omitted means to the end" in
      Arg.(value & opt (some int) None & info ["last"; "l"] ~docv:"INT" ~doc)
    in
    let multi =
      let doc = "Use multipart upload" in
      Arg.(value & flag & info ["multi"; "m"] ~docv:"MULTI" ~doc)
    in

    let chunk_size =
      let doc = "Use streaming get / put the given chunk_size" in
      Arg.(value & opt (some int) None & info ["chunk_size"; "c"] ~docv:"CHUNK_SIZE" ~doc)
    in

    Term.(const make $ common_opts $ first $ last $ multi $ chunk_size $ path 0 "SRC" $ path 1 "DEST"),
    Term.info "cp" ~doc:"Copy files to and from S3"
  in
  let rm =
    let objects =
      let doc = "name of the object to delete" in
      Arg.(non_empty & pos_right 0 string [] & info [] ~docv:"OBJECT" ~doc)
    in

    let make opts bucket paths = opts, Rm { bucket; paths } in
    Term.(const make $ common_opts $ bucket 0 $ objects),
    Term.info "rm" ~doc:"Delete files from s3"
  in

  let head =
    let path =
      let doc = "object: s3://<bucket>/<objname>" in
      Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)
    in

    let make opts path = opts, Head { path } in
    Term.(const make $ common_opts $ path),
    Term.info "head" ~doc:"Head  files from s3"
  in

  let ls =
    let make opts ratelimit prefix bucket = opts, Ls { bucket; prefix; ratelimit } in

    let prefix =
      let doc = "Only list elements with the given prefix" in
      Arg.(value & opt (some string) None & info ["prefix"] ~docv:"PREFIX" ~doc)
    in

    Term.(const make $ common_opts $ ratelimit $ prefix $ bucket 0),
    Term.info "ls" ~doc:"List files in bucket"
  in

  (* Where do the result go? *)
  let help =
    let doc = "Amazon s3 command line interface" in
    let exits = Term.default_exits in
    Term.(ret (const (fun _ -> `Help (`Pager, None)) $ common_opts)),
    Term.info Sys.argv.(0) ~doc ~exits
  in

  let commands =
    let cmds = [cp; rm; ls; head] in
    Term.(eval_choice help cmds)
  in
  let run = function
    | `Ok cmd -> exec cmd
    | _ -> 254
  in
  run @@ commands |> exit
