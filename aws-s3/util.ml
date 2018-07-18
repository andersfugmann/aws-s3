(*{{{
 * Copyright (C) 2015 Trevor Smith <trevorsummerssmith@gmail.com>
 * Copyright (C) 2017 Anders Fugmann <anders@fugmann.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)
open StdLabels
let sprintf = Printf.sprintf
open Cohttp

(* @see https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-streaming.html
   public static String UriEncode(CharSequence input, boolean encodeSlash) {
          StringBuilder result = new StringBuilder();
          for (int i = 0; i < input.length(); i++) {
              char ch = input.charAt(i);
              if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_' || ch == '-' || ch == '~' || ch == '.') {
                  result.append(ch);
              } else if (ch == '/') {
                  result.append(encodeSlash ? "%2F" : ch);
              } else {
                  result.append(toHexUTF8(ch));
              }
          }
          return result.toString();
      }
*)
let encode_string s =
  (* Percent encode the path as s3 wants it. Uri doesn't
       encode $, or the other sep characters in a path.
       If upstream allows that we can nix this function *)
  let n = String.length s in
  let buf = Buffer.create (n * 3) in
  for i = 0 to (n-1) do
    let c = String.get s i in
    match c with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_' | '-' | '~' | '.' | '/' -> Buffer.add_char buf c
    | '%' ->
      (* Sigh. Annoying we're expecting already escaped strings so ignore the escapes *)
      begin
        let is_hex = function
          | 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' -> true
          | _ -> false in
        if (i + 2) < n then
          if is_hex(String.get s (i+1)) && is_hex(String.get s (i+2)) then
            Buffer.add_char buf c
          else
            Buffer.add_string buf "%25"
      end
    | _ -> Buffer.add_string buf (Printf.sprintf "%%%X" (Char.code c))
  done;
  Buffer.contents buf

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

module Make(C : Types.Compat) = struct
  open C
  type body =
  | String of string
  | Empty
  | Chunked of { pipe: string Pipe.reader; length: int; chunk_size: int }

  (* TODO: Calculate sha while retrieving data *)
  let chunked_body ~signing_key ~scope ~initial_signature ~date ~time ~chunk_size reader =
    let open Deferred in
    let open Deferred.Infix in
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
    let send writer previous_signature elements length =
      let sha = Digestif.SHA256.digesti_string
          (fun f -> List.iter ~f elements)
      in
      let signature = Authorization.chunk_signature ~signing_key ~date ~time ~scope
          ~previous_signature ~sha |> Authorization.to_hex in
      let header = Authorization.chunk_header ~length ~signature in
      C.Pipe.write writer header >>= fun () ->
      C.Pipe.write writer "\r\n" >>= fun () ->
      List.fold_left
        ~init:(Deferred.return ()) ~f:(fun x data -> x >>= fun () -> C.Pipe.write writer data)
        elements >>= fun () ->
      C.Pipe.write writer "\r\n" >>= fun () ->
      return signature
    in
    let rec transfer previous_signature (buffered:int) queue current writer =
      begin
        match current with
        | Some v -> return (Some v)
        | None -> C.Pipe.read reader >>= function
          | None -> return None
          | Some v -> return (Some (v, 0))
      end >>= function
      | None ->
        send writer previous_signature (List.rev queue) buffered >>= fun signature ->
        send writer signature [] 0 >>= fun _signature ->
        Deferred.return ()
      | Some (data, offset) -> begin
          let remain = chunk_size - buffered in
          match (String.length data) - offset with
          | n when n >= remain ->
            let elem = sub data ~pos:offset ~len:remain in
            let elements = elem :: (List.rev queue) in
            send writer previous_signature elements chunk_size >>= fun signature ->
            (* Recursive call. *)
            let data = match String.length data > remain with
              | true ->
                Some (data, offset + remain)
              | false ->
                None
            in
            transfer signature 0 [] data writer
          | _ ->
            let elem = sub ~pos:offset data in
            transfer previous_signature
              (buffered + String.length elem)
              (elem :: queue) None writer
        end
    in
    Pipe.create_reader ~f:(transfer initial_signature 0 [] None)

  let make_request ~scheme ?(body=Empty) ?(region=Region.Us_east_1) ?(credentials:Credentials.t option) ~headers
      ~(meth:[ `DELETE | `GET | `HEAD | `POST | `PUT ]) ~path ~query () =
    let host_str = Region.to_host region in
    let (date, time)  = Unix.gettimeofday () |> Time.iso8601_of_time in

    (* Create headers structure *)
    let content_length =
      match meth, body with
      | (`PUT | `POST), String body -> Some (String.length body |> string_of_int)
      | (`PUT | `POST), Chunked { length; chunk_size; _ } ->
        Some (get_chunked_length ~chunk_size length |> string_of_int )
      | (`PUT | `POST), Empty -> Some "0"
      | _ -> None
    in
    let payload_sha = match body with
      | Empty -> empty_sha
      | String body -> Authorization.hash_sha256 body |> Authorization.to_hex
      | Chunked _ -> "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"
    in
    let token = match credentials with
      | Some { Credentials.token ; _ } -> token
      | None -> None
    in

    let headers, body =
      let module HeaderMap = Authorization.HeaderMap in
      let change ~key ~f map =
        match f (HeaderMap.find_opt key map) with
        | None -> HeaderMap.remove key map
        | Some v -> HeaderMap.add key v map
      in
      let add ~key ~data map = HeaderMap.add key data map in
      let add_opt ~key ~data map =
        match data with
        | Some data -> add ~key ~data map
        | None -> map
      in
      let decoded_content_length = match body with
        | Chunked { length; _ } -> Some (string_of_int length)
        | _ -> None
      in
      let headers =
        List.fold_left ~init:HeaderMap.empty ~f:(fun m (k, v) -> HeaderMap.add k v m) headers
        |> add     ~key:"Host"                 ~data:host_str
        |> add     ~key:"x-amz-content-sha256" ~data:payload_sha
        |> add     ~key:"x-amz-date"           ~data:(sprintf "%sT%sZ" date time)
        |> add_opt ~key:"x-amz-security-token" ~data:token
        |> add_opt ~key:"Content-Length"       ~data:content_length
        |> change  ~key:"User-Agent" ~f:(function Some _ as r -> r | None -> Some "aws-s3 ocaml client")
        |> add_opt ~key:"x-amz-decoded-content-length" ~data:decoded_content_length
        |> change  ~key:"Content-Encoding" ~f:(fun v -> match body, v with
            | Chunked _, Some v -> Some ("aws-chunked," ^ v)
            | Chunked _, None -> Some "aws-chunked"
            | _, v -> v)
      in
      let auth, body =
        match credentials with
        | Some credentials ->
          let verb = Code.string_of_method (meth :> Code.meth) in
          let region = Region.to_string region in
          let path = encode_string path in
          let query =
            List.map query ~f:(fun (k, v) -> sprintf "%s=%s" (encode_string k) (encode_string v))
            |> String.concat ~sep:"&"
          in
          let signing_key =
            Authorization.make_signing_key ~date ~region ~service:"s3" ~credentials ()
          in
          let scope = Authorization.make_scope ~date ~region ~service:"s3" in
          let signature, signed_headers =
            Authorization.make_signature ~date ~time ~verb ~path
              ~headers ~query ~scope ~signing_key ~payload_sha
          in
          let auth_header = Authorization.make_auth_header ~credentials ~scope ~signed_headers ~signature in
          let body = match body with
            | String body -> Some (Cohttp_deferred.Body.of_string body)
            | Empty -> None
            | Chunked { pipe; chunk_size; _ } ->
              let pipe =
                chunked_body ~signing_key ~scope
                  ~initial_signature:signature ~date ~time ~chunk_size pipe
              in
              Some (Cohttp_deferred.Body.of_pipe pipe)
          in
          Some auth_header, body


        | None ->
          let body = match body with
            | String body -> Some (Cohttp_deferred.Body.of_string body)
            | Empty -> None
            | Chunked { pipe; _ } -> Some (Cohttp_deferred.Body.of_pipe pipe)
          in
          None, body
      in
      add_opt ~key:"Authorization" ~data:auth headers, body
    in

    let uri = Uri.make
        ~scheme:(match scheme with `Http -> "http"
                                 | `Https -> "https")
        ~host:host_str
        ~path
        ~query:(List.map ~f:(fun (k,v) -> k, [v]) query)
        ()
    in

    Cohttp_deferred.call
      ~headers:(Header.of_list (Authorization.HeaderMap.bindings headers))
      ?body
      (meth :> Code.meth)
      uri

end
