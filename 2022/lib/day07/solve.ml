open Core
open Parse

type state_t =
  { path : string list
  ; root : Tree.t
  }

let all_but_last l =
  let backwards =
    match List.rev l with
    | [] -> []
    | _ :: tl -> tl
  in
  List.rev backwards
;;

let add_cd state cd =
  match cd with
  | Root_dir -> { state with path = [] }
  | Up_a_dir -> { state with path = all_but_last state.path }
  | Down_a_dir dirname ->
    let path = List.concat [ state.path; [ dirname ] ] in
    { state with path }
;;

let add_output state output =
  match output with
  | File file ->
    let root = Tree.add_file state.root state.path file in
    { state with root }
  | Dir dirname ->
    let root = Tree.add_dir state.root state.path (Tree.empty_tree dirname) in
    { state with root }
;;

let add_outputs state outputs = List.fold outputs ~init:state ~f:add_output

let add_parsed state parsed =
  match parsed with
  | Cmd cmd ->
    (match cmd with
     | Ls -> state
     | Cd cd -> add_cd state cd)
  | Outputs outputs -> add_outputs state outputs
;;

let build_tree filename =
  let aux ic =
    let init = { root = Tree.empty_tree "/"; path = [] } in
    let final_state = read_lines ic |> Sequence.fold ~init ~f:add_parsed in
    final_state.root
  in
  In_channel.with_file filename ~f:aux
;;

let build_sized_tree = build_tree %> Tree.build_sized

let build_and_print filename =
  let sexp = build_tree filename |> Tree.build_sized |> Tree.sexp_of_t in
  let formatter = Format.formatter_of_out_channel Stdio.stdout in
  Sexplib.Sexp.pp_hum formatter sexp
;;

let build_and_get_size filename = build_tree filename |> Tree.get_size
