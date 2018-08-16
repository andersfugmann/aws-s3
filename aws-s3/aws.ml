open StdLabels
let sprintf = Printf.sprintf
let debug = false
let log fmt = match debug with
  | true -> Printf.kfprintf (fun _ -> ()) stderr ("%s: " ^^ fmt ^^ "\n%!") __MODULE__
  | false -> Printf.ikfprintf (fun _ -> ()) stderr fmt
let _ = log

let empty_sha = Authorization.hash_sha256 "" |> Authorization.to_hex

let get_chunked_length ~chunk_size payload_length =
  let lengths =
    (sprintf "%x" chunk_size |> String.length) * (payload_length / chunk_size) +
    (match payload_length mod chunk_size with
     | 0 -> 0
     | n -> (sprintf "%x" n |> String.length))
  in
  let chunks = (payload_length + (chunk_size - 1)) / chunk_size + 1 in
  chunks * (4 + 17 + 64) + lengths + 1 +
  payload_length

let%test "get_chunk_length" =
  get_chunked_length ~chunk_size:(64*1024) (65*1024) = 66824

module Make(Io : Types.Io) = struct
  module Body = Body.Make(Io)
  module Http = Http.Make(Io)
  open Io
  open Deferred

  let chunk_writer ~signing_key ~scope ~initial_signature ~date ~time ~chunk_size reader =
    let open Deferred in
    let sub ~pos ?len str =
      let res = match pos, len with
        | 0, None -> str
        | 0, Some len when len = String.length str -> str
        | pos, None ->
          String.sub ~pos ~len:(String.length str - pos) str
        | pos, Some len ->
          String.sub ~pos ~len str
      in
      res
    in
    let send writer sha previous_signature elements length =
      let flush_done = Pipe.flush writer in
      let sha = Digestif.SHA256.get sha in
      let signature = Authorization.chunk_signature ~signing_key ~date ~time ~scope
          ~previous_signature ~sha |> Authorization.to_hex in
      Io.Pipe.write writer
        (Printf.sprintf "%x;chunk-signature=%s\r\n" length signature) >>= fun () ->
      List.fold_left
        ~init:(Deferred.return ()) ~f:(fun x data -> x >>= fun () -> Io.Pipe.write writer data)
        elements >>= fun () ->
      Io.Pipe.write writer "\r\n" >>= fun () ->
      flush_done >>= fun () ->
      return signature
    in
    let rec transfer previous_signature ctx (buffered:int) queue current writer =
      begin
        match current with
        | Some v -> return (Some v)
        | None ->
          Io.Pipe.read reader >>= function
          | None ->
            return None
          | Some v ->
            return (Some (v, 0))
      end >>= function
      | None ->
        send writer ctx previous_signature (List.rev queue) buffered >>= fun signature ->
        send writer Digestif.SHA256.empty signature [] 0 >>= fun _signature ->
        Deferred.return ()
      | Some (data, offset) -> begin
          let remain = chunk_size - buffered in
          match (String.length data) - offset with
          | n when n >= remain ->
            let elem = sub data ~pos:offset ~len:remain in
            let ctx = Digestif.SHA256.feed_string ctx elem in
            let elements = elem :: queue |> List.rev in
            send writer ctx previous_signature elements chunk_size >>= fun signature ->
            (* Recursive call. *)
            let data = match String.length data > remain with
              | true ->
                Some (data, offset + remain)
              | false ->
                None
            in
            transfer signature Digestif.SHA256.empty 0 [] data writer
          | _ ->
            let elem = sub ~pos:offset data in
            let ctx = Digestif.SHA256.feed_string ctx elem in
            transfer previous_signature ctx
              (buffered + String.length elem)
              (elem :: queue) None writer
        end
    in
    Pipe.create_reader ~f:(transfer initial_signature Digestif.SHA256.empty 0 [] None)

  let make_request ?(domain=Unix.PF_INET) ~scheme ?(expect=false) ~sink ?(body=Body.Empty) ?(region=Region.Us_east_1) ?(credentials:Credentials.t option) ~headers ~meth ~path ~query () =
    let dualstack = match domain with
      | Unix.PF_INET -> false
      | Unix.PF_INET6 -> true
      | Unix.PF_UNIX -> failwith "Unix domain calls are not supported"
    in
    let host_str = Region.to_host ~dualstack region in
    let (date, time)  = Unix.gettimeofday () |> Time.iso8601_of_time in

    (* Create headers structure *)
    let content_length =
      match meth, body with
      | (`PUT | `POST), Body.String body -> Some (String.length body |> string_of_int)
      | (`PUT | `POST), Body.Chunked { length; chunk_size; _ } ->
        Some (get_chunked_length ~chunk_size length |> string_of_int )
      | (`PUT | `POST), Body.Empty -> Some "0"
      | _ -> None
    in
    let payload_sha = match body with
      | Body.Empty -> empty_sha
      | Body.String body -> Authorization.hash_sha256 body |> Authorization.to_hex
      | Body.Chunked _ -> "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"
    in
    let token = match credentials with
      | Some { Credentials.token ; _ } -> token
      | None -> None
    in

    let headers, body =
      let decoded_content_length = match body with
        | Body.Chunked { length; _ } -> Some (string_of_int length)
        | _ -> None
      in
      let open Headers in
      let headers =
        List.fold_left ~init:empty ~f:(fun m (key, value) -> add ~key ~value m) headers
        |> add     ~key:"Host"                 ~value:host_str
        |> add     ~key:"x-amz-content-sha256" ~value:payload_sha
        |> add     ~key:"x-amz-date"           ~value:(sprintf "%sT%sZ" date time)
        |> add_opt ~key:"x-amz-security-token" ~value:token
        |> add_opt ~key:"Content-Length"       ~value:content_length
        |> change  ~key:"User-Agent" ~f:(function Some _ as r -> r | None -> Some "aws-s3 ocaml client")
        |> add_opt ~key:"x-amz-decoded-content-length" ~value:decoded_content_length
        |> change  ~key:"Content-Encoding" ~f:(fun v -> match body, v with
            | Body.Chunked _, Some v -> Some ("aws-chunked," ^ v)
            | Body.Chunked _, None -> Some "aws-chunked"
            | _, v -> v)
      in

      let auth, body =
        match credentials with
        | Some credentials ->
          let verb = Http.string_of_method meth in
          let region = Region.to_string region in
          let signing_key =
            Authorization.make_signing_key ~date ~region ~service:"s3" ~credentials ()
          in
          let scope = Authorization.make_scope ~date ~region ~service:"s3" in
          let signature, signed_headers =
            Authorization.make_signature ~date ~time ~verb ~path
              ~headers ~query:query ~scope ~signing_key ~payload_sha
          in
          let auth = (Authorization.make_auth_header ~credentials ~scope ~signed_headers ~signature) in
          let body = match body with
            | Body.String body ->
              let reader, writer = Pipe.create () in
              Pipe.write writer body >>= fun () ->
              Pipe.close writer;
              return (Some reader)
            | Body.Empty -> return None
            | Body.Chunked { pipe; chunk_size; _ } ->
              let pipe =
                (* Get errors if the chunk_writer fails *)
                chunk_writer ~signing_key ~scope
                  ~initial_signature:signature ~date ~time ~chunk_size pipe
              in
              return (Some pipe)
          in
          Some auth, body
        | None ->
          let body = match body with
            | Body.String body ->
              let reader, writer = Pipe.create () in
              Pipe.write writer body >>= fun () ->
              Pipe.close writer;
              return (Some reader)
            | Body.Empty -> return None
            | Body.Chunked { pipe; _} ->
              return (Some pipe)
          in
          None, body
      in
      (Headers.add_opt ~key:"Authorization" ~value:auth headers), body
    in
    body >>= fun body ->
    Http.call ~domain ~scheme ~host:host_str ~path ~query ~headers ~expect ~sink ?body meth >>=? fun (code, msg, headers, body) ->
    Deferred.Or_error.return (code, msg, headers, body)
end
