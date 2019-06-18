open !StdLabels [@@warning "-66"]
let sprintf = Printf.sprintf

(* Use ptime for time conversions. This is error prone as we fiddle with the environment *)
let parse_iso8601_string str =
  (*
  Scanf.sscanf str "%d-%d-%dT%d:%d:%d.%s"
    (fun year month day hour min sec _frac ->
       match Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0)) with
       | None -> failwith "Illegal time format"
       | Some t -> Ptime.to_float_s t)
  *)
  Ptime.of_rfc3339 str
  |> function
  | Ok (t, _, _) -> Ptime.to_float_s t
  | Error _ -> failwith "Could not parse date string"

let%test _ =
  parse_iso8601_string "2018-02-27T13:39:35.000Z" = 1519738775.0

let parse_rcf1123_string date_str =
  let int_of_month = function
    | "Jan" -> 1
    | "Feb" -> 2
    | "Mar" -> 3
    | "Apr" -> 4
    | "May" -> 5
    | "Jun" -> 6
    | "Jul" -> 7
    | "Aug" -> 8
    | "Sep" -> 9
    | "Oct" -> 10
    | "Nov" -> 11
    | "Dec" -> 12
    | _ -> failwith "Unknown month"
  in
  Scanf.sscanf date_str "%s %d %s %d %d:%d:%d %s"
    (fun _dayname day month_str year hour min sec _zone ->
       let month = int_of_month month_str in
       match Ptime.of_date_time ((year, month, day), ((hour, min, sec), 0)) with
       | None -> failwith "Illegal time format"
       | Some t -> Ptime.to_float_s t)

let%test _ =
  parse_rcf1123_string "Mon, 16 Jul 2018 10:31:41 GMT" = 1531737101.0

let iso8601_of_time time =
  let t =
    Ptime.of_float_s time
    |> function Some t -> t | None -> failwith "Time out of range"
  in

  let (year, month, day), ((hour, min, sec), _) = Ptime.to_date_time t in
  let date_str = sprintf "%.4d%.2d%.2d" year month day in
  let time_str = sprintf "%.2d%.2d%.2d" hour min sec in
  (date_str, time_str)

let%test _ =
  let t = 1369353600.0 in
  ("20130524", "000000") = iso8601_of_time t
