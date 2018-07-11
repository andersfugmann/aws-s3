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

open Core
open Cohttp

let yyymmdd_of_time time =
  Date.of_time ~zone:Time.Zone.utc time
  |> Date.to_string_iso8601_basic

let iso8601_of_time time =
  let {Time.Span.Parts.hr; min; sec; _} =
    Time.to_ofday ~zone:Time.Zone.utc time
    |> Time.Ofday.to_parts
  in
  (yyymmdd_of_time time, sprintf "%.2d%.2d%.2d" hr min sec)

let%test _ =
  let date, time = Time.(Span.of_int_sec 1369353600 |> of_span_since_epoch)
                   |> iso8601_of_time
  in
  "20130524T000000Z" = sprintf "%sT%sZ" date time

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
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
          | _ -> false in
        if (i + 2) < n then
          if is_hex(String.get s (i+1)) && is_hex(String.get s (i+2)) then
            Buffer.add_char buf c
          else
            Buffer.add_string buf "%25"
      end
    | _ -> Buffer.add_string buf (Printf.sprintf "%%%X" (Char.to_int c))
  done;
  Buffer.contents buf

let empty_sha = Authorization.hash_sha256 "" |> Authorization.to_hex

module Make(C : Types.Compat) = struct
  open C
  let make_request ~scheme ?body ?(region=Region.Us_east_1) ?(credentials:Credentials.t option) ~headers
      ~(meth:[ `DELETE | `GET | `HEAD | `POST | `PUT ]) ~path ~query () =
    let host_str = Region.to_host region in
    let (date, time)  = Time.now () |> iso8601_of_time in

    (* Create headers structure *)
    let content_length =
      match meth with
      | `PUT | `POST -> Option.value_map ~f:(String.length) ~default:0 body |> string_of_int |> Option.some
      | _ -> None
    in
    let payload_sha = match body with
      | None -> empty_sha
      | Some body -> Authorization.hash_sha256 body |> Authorization.to_hex
    in
    let token = match credentials with
      | Some { Credentials.token ; _ } -> token
      | None -> None
    in

    let headers =
      let change ~key ~f map = Authorization.HeaderMap.change map key ~f in
      let add ~key ~data map =
        match Authorization.HeaderMap.add map ~key ~data with
        | `Duplicate -> failwith "Duplicate header fields not supported."
        | `Ok map -> map
      in
      let add_opt ~key ~data map = Option.value_map ~default:map ~f:(fun data -> add ~key ~data map) data in

      let headers =
        Authorization.HeaderMap.of_alist_exn headers
        |> add     ~key:"Host"                 ~data:host_str
        |> add     ~key:"x-amz-content-sha256" ~data:payload_sha
        |> add     ~key:"x-amz-date"           ~data:(sprintf "%sT%sZ" date time)
        |> add_opt ~key:"x-amz-security-token" ~data:token
        |> add_opt ~key:"Content-Length"       ~data:content_length
        |> change  ~key:"User-Agent" ~f:(function Some _ as r -> r | None -> Some "aws-s3 ocaml client")
      in
      let auth = Option.map credentials ~f:(fun credentials ->
          let verb = Code.string_of_method (meth :> Code.meth) in
          let region = Region.to_string region in
          let path = encode_string path in
          let query =
            List.map query ~f:(fun (k, v) -> sprintf "%s=%s" (encode_string k) (encode_string v))
            |> String.concat ~sep:"&"
          in
          Authorization.make_authorization
            ~date ~time ~verb ~credentials ~path ~headers
            ~query ~region ~service:"s3" ~payload_sha:payload_sha
        )
      in
      add_opt ~key:"Authorization" ~data:auth headers
    in

    let uri = Uri.make
        ~host:host_str
        ~path
        ~query:(List.map ~f:(fun (k,v) -> k, [v]) query)
        ()
    in

    let request = Request.make ~meth:(meth :> Code.meth)
        ~headers:(Header.of_list (Authorization.HeaderMap.to_alist headers))
        uri
    in
    (* Dont use request at all. *)
    match meth with
    | `PUT
    | `POST ->
      let body = Option.map ~f:(Cohttp_deferred.Body.of_string) body in
      Cohttp_deferred.Client.request ~scheme ?body request
    | `GET
    | `DELETE
    | `HEAD -> Cohttp_deferred.Client.request ~scheme request
end
