open !StdLabels
let sprintf = Printf.sprintf
let parse_iso8601_string str =
  let prev_tz = try Unix.unsafe_getenv "TZ" with Not_found -> "" in
  Unix.putenv "TZ" "GMT";

  let time = Scanf.sscanf str "%d-%d-%dT%d:%d:%fZ"
      (fun year month tm_mday tm_hour tm_min sec ->
         let tm_mon = month - 1 in
         let tm_year = year - 1900 in
         let res =
           { Unix.tm_sec = 0; tm_min; tm_hour; tm_mday; tm_mon; tm_year;
             tm_wday = 0; tm_yday = 0; tm_isdst = false; (* These are ignored *) }
           |> Unix.mktime
           |> fst
         in
         res +. sec)
  in
  Unix.putenv "TZ" prev_tz;
  time

let%test _ =
  parse_iso8601_string "2018-02-27T13:39:35.000Z" = 1519738775.0

let parse_rcf1123_string date_str =
  let int_of_month = function
    | "Jan" -> 0
    | "Feb" -> 1
    | "Mar" -> 2
    | "Apr" -> 3
    | "May" -> 4
    | "Jun" -> 5
    | "Jul" -> 6
    | "Aug" -> 7
    | "Sep" -> 8
    | "Oct" -> 9
    | "Nov" -> 10
    | "Dec" -> 11
    | _ -> failwith "Unknown month"
  in
  let prev_tz = try Unix.unsafe_getenv "TZ" with Not_found -> "" in
  Unix.putenv "TZ" "GMT";
  let r = Scanf.sscanf date_str "%s %d %s %d %d:%d:%d %s"
      (fun _dayname tm_mday month_str year tm_hour tm_min tm_sec _zone ->
         let tm_mon = int_of_month month_str in
         let tm_year = year - 1900 in
         { Unix.tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year;
           tm_wday = 0; tm_yday = 0; tm_isdst = false; (* These are ignored *) }
         |> Unix.mktime
         |> fst)
  in
  Unix.putenv "TZ" prev_tz;
  r


let%test _ =
  parse_rcf1123_string "Mon, 16 Jul 2018 10:31:41 GMT" = 1531737101.0

let iso8601_of_time time =
  let
    { Unix.tm_sec; tm_min; tm_hour;
      tm_mday; tm_mon; tm_year; _ } = Unix.gmtime time
  in
  let date_str = sprintf "%.4d%.2d%.2d" (tm_year + 1900) (tm_mon + 1) tm_mday in
  let time_str = sprintf "%.2d%.2d%.2d" tm_hour tm_min tm_sec in
  (date_str, time_str)

let%test _ =
  let t = 1369353600.0 in
  ("20130524", "000000") = iso8601_of_time t
