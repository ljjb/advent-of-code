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

let quantum_resolve workflows (lo, hi) =
  let q = Queue.create () in
  let init = "in", Quantum_part.init (lo, hi) in
  Queue.enqueue q init;
  let rec loop tally =
    match Queue.dequeue q with
    | None -> tally
    | Some (workflow_name, quantum_part) ->
      let workflow = Map.find_exn workflows workflow_name in
      let delta, forwarded_parts = Quantum_part.split_workflow quantum_part workflow in
      Map.iteri forwarded_parts ~f:(fun ~key ~data -> Queue.enqueue q (key, data));
      loop (tally + delta)
  in
  loop 0
;;

let part2 filename =
  let workflows, _ = read_puzzle filename in
  quantum_resolve workflows (1, 4000) |> sprintf !"%{sexp:(int)}"
;;
