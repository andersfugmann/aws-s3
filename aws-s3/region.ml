open Core
type t =
 | Ap_northeast_1  (* Asia Pacific (Tokyo) *)
 | Ap_northeast_2  (* Asia Pacific (Seoul) *)
 | Ap_northeast_3  (* Asia Pacific (Osaka-Local) *)
 | Ap_southeast_1  (* Asia Pacific (Singapore) *)
 | Ap_southeast_2  (* Asia Pacific (Sydney) *)
 | Ap_south_1      (* Asia Pacific (Mumbai) *)
 | Eu_central_1    (* EU (Frankfurt) *)
 | Cn_northwest_1  (* China (Ningxia)        *)
 | Cn_north_1      (* China (Beijing)        *)
 | Eu_west_1       (* EU (Ireland)   *)
 | Eu_west_2       (* EU (London)    *)
 | Eu_west_3       (* EU (Paris)     *)
 | Sa_east_1       (* South America (São Paulo)      *)
 | Us_east_1       (* US East (N. Virginia) *)
 | Us_east_2       (* US East (Ohio) *)
 | Us_west_1       (* US West (N. California) *)
 | Us_west_2       (* US West (Oregon) *)
 | Ca_central_1    (* Canada - central *)
 | Other of string (* Other unknown *)

let to_string = function
  | Ap_northeast_1 -> "ap-northeast-1"
  | Ap_northeast_2 -> "ap-northeast-2"
  | Ap_northeast_3 -> "ap-northeast-3"
  | Ap_southeast_1 -> "ap-southeast-1"
  | Ap_southeast_2 -> "ap-southeast-2"
  | Ap_south_1     -> "ap-south-1"
  | Eu_central_1   -> "eu-central-1"
  | Cn_northwest_1 -> "cn-northwest-1"
  | Cn_north_1     -> "cn-north-1"
  | Eu_west_1      -> "eu-west-1"
  | Eu_west_2      -> "eu-west-2"
  | Eu_west_3      -> "eu-west-3"
  | Sa_east_1      -> "sa-east-1"
  | Us_east_1      -> "us-east-1"
  | Us_east_2      -> "us-east-2"
  | Us_west_1      -> "us-west-1"
  | Us_west_2      -> "us-west-2"
  | Ca_central_1   -> "ca-central-1"
  | Other s        -> s

let of_string = function
  | "ap-northeast-1" -> Ap_northeast_1
  | "ap-northeast-2" -> Ap_northeast_2
  | "ap-northeast-3" -> Ap_northeast_3
  | "ap-southeast-1" -> Ap_southeast_1
  | "ap-southeast-2" -> Ap_southeast_2
  | "ap-south-1"     -> Ap_south_1
  | "eu-central-1"   -> Eu_central_1
  | "cn-northwest-1" -> Cn_northwest_1
  | "cn-north-1"     -> Cn_north_1
  | "eu-west-1"      -> Eu_west_1
  | "eu-west-2"      -> Eu_west_2
  | "eu-west-3"      -> Eu_west_3
  | "sa-east-1"      -> Sa_east_1
  | "us-east-1"      -> Us_east_1
  | "us-east-2"      -> Us_east_2
  | "us-west-1"      -> Us_west_1
  | "us-west-2"      -> Us_west_2
  | "ca-central-1"   -> Ca_central_1
  | s                -> failwith ("Unknown region: " ^ s)


let of_host host =
  match String.split ~on:'.' host |> List.rev with
  | "com" :: "amazonaws" :: "s3" :: _  ->
    Us_east_1
  | "com" :: "amazonaws" :: host :: _ ->
    String.chop_prefix host ~prefix:"s3-"
    |> Option.value ~default:host
    |> of_string
  | _ -> failwith "Cannot parse region from host"

let to_host region =
    to_string region |> sprintf "s3.%s.amazonaws.com"
