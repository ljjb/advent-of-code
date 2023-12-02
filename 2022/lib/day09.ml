open Core
open Stdio
module Step = Sequence.Step

type dir_t =
  | Left
  | Right
  | Up
  | Down
[@@deriving sexp_of]

module Pos = struct
  module T = struct
    type t =
      { x : int
      ; y : int
      }
    [@@deriving sexp_of]

    let compare t1 t2 =
      match Int.compare t1.x t2.x with
      | 0 -> Int.compare t1.y t2.y
      | n -> n
    ;;
  end

  include T
  include Comparator.Make (T)
end

let parse_dir = function
  | "L" -> Some Left
  | "R" -> Some Right
  | "D" -> Some Down
  | "U" -> Some Up
  | _ -> None
;;

let dir_seq_of_instr dir num = Sequence.init num ~f:(fun _ -> dir)

let parse_line line =
  match String.split ~on:' ' line with
  | dir :: n :: _ ->
    let dir_opt = parse_dir dir in
    let num_opt = Int.of_string_opt n in
    Option.map2 dir_opt num_opt ~f:(fun dir num -> dir_seq_of_instr dir num)
  | _ -> None
;;

let read_puzzle ic =
  let f ic =
    match In_channel.input_line ic with
    | Some line ->
      (match parse_line line with
       | Some instr -> Step.Yield { value = instr; state = ic }
       | None -> Step.Skip { state = ic })
    | None -> Step.Done
  in
  Sequence.unfold_step ~init:ic ~f |> Sequence.concat
;;

let move pos = function
  | Left -> Pos.{ pos with x = pos.x - 1 }
  | Right -> { pos with x = pos.x + 1 }
  | Up -> { pos with y = pos.y - 1 }
  | Down -> { pos with y = pos.y + 1 }
;;

let move_delta delta tail =
  if delta > 0 then tail + 1 else if delta < 0 then tail - 1 else tail
;;

let within_range dx dy = Int.abs dx <= 1 && Int.abs dy <= 1

let follow head tail =
  let open Pos in
  let dx = head.x - tail.x in
  let dy = head.y - tail.y in
  if within_range dx dy
  then tail
  else { x = move_delta dx tail.x; y = move_delta dy tail.y }
;;

type state_t =
  { visited : Set.M(Pos).t
  ; head : Pos.t
  ; tails : Pos.t list
  }

let starting_pos () = Pos.{ x = 0; y = 0 }

let starting_state n =
  { visited = Set.empty (module Pos)
  ; head = starting_pos ()
  ; tails = List.init n ~f:(fun _ -> starting_pos ())
  }
;;

let update state dir =
  let head = move state.head dir in
  let tail_tail, tails =
    List.fold_map state.tails ~init:head ~f:(fun head tail ->
      let tail = follow head tail in
      tail, tail)
  in
  let visited = Set.add state.visited tail_tail in
  { visited; head; tails }
;;

let solve_puzzle ?(n = 1) filename =
  let aux ic =
    let moves = read_puzzle ic in
    let init = starting_state n in
    let final_state = Sequence.fold moves ~init ~f:update in
    Set.length final_state.visited
  in
  In_channel.with_file filename ~f:aux
;;

let part1 = solve_puzzle ~n:1
let part2 = solve_puzzle ~n:9
