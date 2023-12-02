open Core
open Stdio

let id x = x
let ( %> ) g f x = x |> g |> f

type operation_t =
  | Noop
  | Add_x of int
[@@deriving sexp_of]

type add_t = { value : int } [@@deriving sexp_of]

let parse_line line =
  let chunks = String.split ~on:' ' line in
  match chunks with
  | "noop" :: _ -> Some Noop
  | "addx" :: int_string :: _ ->
    Int.of_string_opt int_string |> Option.map ~f:(fun x -> Add_x x)
  | _ -> None
;;

let read_puzzle ic =
  let module Step = Sequence.Step in
  let f ic =
    match In_channel.input_line ic with
    | None -> Step.Done
    | Some line ->
      (match parse_line line with
       | Some parsed -> Step.Yield { value = parsed; state = ic }
       | None -> Step.Skip { state = ic })
  in
  Sequence.unfold_step ~init:ic ~f
;;

let prepend seq ~item = Sequence.of_list [ Sequence.return item; seq ] |> Sequence.concat
let to_check = Set.of_list (module Int) [ 20; 60; 100; 140; 180; 220 ]

let get_states seq =
  let init = 1 in
  let f in_state operation =
    let out_state, states =
      match operation with
      | Noop -> in_state, [ in_state ]
      | Add_x x ->
        let out_state = in_state + x in
        out_state, [ in_state; out_state ]
    in
    out_state, Sequence.of_list states
  in
  Sequence.folding_map seq ~init ~f |> Sequence.concat |> prepend ~item:1
;;

let within_range i j = Int.(abs (i - j) <= 1)
let chunks_exn seq ~n = Sequence.chunks_exn seq n

let sum_strengths =
  Sequence.filter_mapi ~f:(fun i register ->
    if Set.mem to_check (i + 1) then Some ((i + 1) * register) else None)
  %> Sequence.sum (module Int) ~f:id
;;

let print_rows =
  Sequence.mapi ~f:(fun i x -> if within_range (i mod 40) x then '#' else '.')
  %> chunks_exn ~n:40
  %> Sequence.map ~f:String.of_char_list
  %> Sequence.to_list
  %> String.concat ~sep:"\n"
;;

let solve filename ~solver =
  let f = read_puzzle %> get_states %> solver in
  In_channel.with_file filename ~f
;;

let part1 = solve ~solver:sum_strengths
let part2 = solve ~solver:print_rows
