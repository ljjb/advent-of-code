open Core
open Stdio
open Components

let read_puzzle filename =
  In_channel.read_lines filename
  |> List.group ~break:(fun _ y -> String.is_empty y)
  |> function
  | workflows :: machine_parts :: _ ->
    ( List.map workflows ~f:Parse.parse_workflow_with_name
      |> Map.of_alist_exn (module String)
    , List.filter machine_parts ~f:(fun a -> not @@ String.is_empty a)
      |> List.map ~f:Parse.parse_machine_part )
  | _ -> failwith "invalid input!"
;;

let transition workflow part =
  List.find_map workflow.Workflow.rules ~f:(Rule.destination_opt ~part)
  |> Option.value ~default:workflow.default
;;

let resolve ~workflows part ~init =
  let rec aux cur =
    let workflow = Map.find_exn workflows cur in
    match transition workflow part with
    | `Verdict accepted -> if accepted then Machine_part.rating part else 0
    | `Workflow nxt -> aux nxt
  in
  aux init
;;

let part1 filename =
  let workflows, parts = read_puzzle filename in
  List.map parts ~f:(resolve ~workflows ~init:"in")
  |> List.sum (module Int) ~f:Util.id
  |> sprintf !"%{sexp:(int)}"
;;
