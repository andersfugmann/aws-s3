(** Parse command line options *)
open Cmdliner

(* When executing, we take a function to do the work,
   so we need a descr of what to do.
*)

type actions =
  | Ls of { bucket: string; prefix: string option; ratelimit: int option; }
  | Rm of string
  | Cp of { src: string; dest: string }

type options =
  { profile: string option; }

let parse exec =
  let profile =
    let doc = "Specify profile to use." in
    Arg.(value & opt (some string) None & info ["profile"; "p"] ~docv:"PROFILE" ~doc)
  in

  let ratelimit =
    let doc = "Limit requests to N/sec." in
    Arg.(value & opt (some int) None & info ["ratelimit"; "r"] ~docv:"RATELIMIT" ~doc)
  in

  let common_opts =
    let make profile = { profile } in
    Term.(const make $ profile)
  in

  let bucket n =
    let doc = "S3 bucket name" in
    Arg.(required & pos n (some string) None & info [] ~docv:"BUCKET" ~doc)
  in

  let prefix n =
    let doc = "Only list elements with the given prefix" in
    Arg.(value & pos n (some string) None & info [] ~docv:"PREFIX" ~doc)
  in


  let path n name =
    let doc = "path: <local_path>|<s3://<bucket>/<objname>" in
    Arg.(required & pos n (some string) None & info [] ~docv:name ~doc)
  in

  let cp =
    let make opts src dest = opts, Cp { src; dest } in
    Term.(const make $ common_opts $ path 0 "SRC" $ path 1 "DEST"),
    Term.info "cp" ~doc:"Copy files to and from S3"
  in
  let rm =
    let make opts path = opts, Rm path in
    Term.(const make $ common_opts $ path 0 "PATH"),
    Term.info "rm" ~doc:"Delete files from s3"
  in

  let ls =
    let make opts ratelimit bucket prefix = opts, Ls { bucket; prefix; ratelimit } in
    Term.(const make $ common_opts $ ratelimit $ bucket 0 $ prefix 1),
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
    let cmds = [cp; rm; ls] in
    Term.(eval_choice help cmds)
  in
  exec @@ commands
