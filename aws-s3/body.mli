module Make(Io : Types.Io) : sig
  open Io
  open Deferred
  type t = string Pipe.reader
  type body =
    | String of string
    | Empty
    | Chunked of { pipe : string Pipe.reader; length : int; chunk_size : int; }
  val to_string : ?length:int -> string Pipe.reader -> string Deferred.t
  val read_string :
    length:int ->
    string option ->
    string Pipe.reader -> (string * string option) Or_error.t
  val of_pipe :
    length:int -> ?start:string -> string Pipe.reader -> string Pipe.reader
  val read_until :
    sep:string ->
    string Pipe.reader -> string -> (string * string, exn) result Deferred.t
  val chunk_reader : string Pipe.reader -> string -> string Pipe.reader
end
