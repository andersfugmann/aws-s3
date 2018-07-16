module Deferred = struct
  type 'a t = 'a

  module Or_error = struct
    type nonrec 'a t = ('a, exn) result
    let return v = Ok v
    let fail exn = Error exn
    let catch f =
      try
        f ()
      with
        exn -> fail exn

    module Infix = struct
      let (>>=) a b =
        match a with
        | Ok v -> b v
        | Error _ as e -> e
    end
  end

  module Infix = struct
    let (>>=) a b = b a
    let (>>|) a b = b a
    let (>>=?) = Or_error.Infix.(>>=)
  end

  let return a = a
  let after delay = Thread.delay delay |> ignore
  let catch f = Or_error.catch (fun () -> f () |> Or_error.return)
end

module Cohttp_deferred = struct
  module Body = struct
    type t = Cohttp.Body.t
    let to_string = Cohttp.Body.to_string
    let of_string = Cohttp.Body.of_string
  end

  module Client = struct
    let request ~scheme:_ ?body:_ _request = failwith "Not implemented"
  end
end
