open Core
open Stdio
open Angstrom

let beacon_sensor =
  let ci = string_ci in
  let mc = many (char ' ') in
  let integer =
    take_while1 (fun c -> Char.is_digit c || Char.equal c '-') >>| Int.of_string
  in
  let xy =
    let* x = ci "x=" *> integer in
    let* y = char ',' *> mc *> ci "y=" *> integer in
    return (x, y)
  in
  let* sensor = ci "sensor" *> mc *> ci "at" *> mc *> xy in
  let* beacon =
    char ':'
    *> mc
    *> ci "closest"
    *> mc
    *> ci "beacon"
    *> mc
    *> ci "is"
    *> mc
    *> ci "at"
    *> mc
    *> xy
  in
  return (sensor, beacon)
;;

let parse_line line =
  match parse_string ~consume:All beacon_sensor line with
  | Ok parsed -> Some parsed
  | _ -> None
;;

let parse filename = In_channel.read_lines filename |> List.filter_map ~f:parse_line
