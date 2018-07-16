open Lwt.Infix
module Deferred = struct
  type 'a t = 'a Lwt.t

  module Or_error = struct
    type nonrec 'a t = ('a, exn) Lwt_result.t
    let return = Lwt_result.return
    let fail = Lwt_result.fail
    let catch : (unit -> 'a t) -> 'a t = fun f ->
      Lwt.catch f Lwt_result.fail

    module Infix = struct
      let (>>=) a b =
        a >>= function
        | Ok v -> b v
        | Error _ as err -> Lwt.return err
    end
  end

  module Infix = struct
    let (>>=) = Lwt.Infix.(>>=)
    let (>>|) a b = a >>= fun v -> Lwt.return (b v)
    let (>>=?) = Lwt_result.Infix.(>>=)
  end

  let return = Lwt.return
  let after delay = Lwt_unix.sleep delay
  let catch f =
    Lwt.catch
      (fun () -> f () >>= Or_error.return)
      (fun exn -> Or_error.fail exn)
end

module Cohttp_deferred = struct
  module Body = struct
    type t = Cohttp_lwt.Body.t
    let to_string = Cohttp_lwt.Body.to_string
    let of_string = Cohttp_lwt.Body.of_string
  end

  module Client = struct
    let request ~scheme ?body request =
      let scheme_str = match scheme with
        | `Http -> "http";
        | `Https -> "https";
      in
      let uri = Uri.with_uri ~scheme:(Some scheme_str) (Cohttp.Request.uri request) in
      Cohttp_lwt_unix.Client.call
        ~headers:(Cohttp.Request.headers request)
        ?body
        (Cohttp.Request.meth request)
        uri

  end
end
