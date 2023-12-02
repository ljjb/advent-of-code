open Core
module Step = Sequence.Step

let id x = x
let ( %> ) f g x = x |> f |> g
let ( <% ) f g x = x |> g |> f

type move_t =
  { n : int
  ; src : int
  ; dst : int
  }

let get_move_opt line =
  let chunks =
    line
    |> String.split_on_chars ~on:[ ' '; '\t' ]
    |> List.filter ~f:(String.is_empty %> not)
  in
  match chunks with
  | "move" :: n_str :: "from" :: src_str :: "to" :: dst_str :: _ ->
    let n = Int.of_string n_str
    and src = Int.of_string src_str - 1
    and dst = Int.of_string dst_str - 1 in
    Some { n; src; dst }
  | _ -> None
;;

let get_moves init =
  let f ic =
    match In_channel.input_line ic with
    | None ->
      In_channel.close ic;
      Step.Done
    | Some line when String.is_prefix line ~prefix:"move" ->
      (match get_move_opt line with
       | None -> Step.Skip { state = ic }
       | Some move -> Step.Yield { state = ic; value = move })
    | _ -> Step.Skip { state = ic }
  in
  Sequence.unfold_step ~init ~f
;;

let get_piles_n_exn lines =
  let len = lines |> List.hd_exn |> String.length in
  (len + 1) / 4
;;

let get_row_char_opts line =
  let f i c =
    if (i - 1) mod 4 = 0 then Some (if Char.is_alpha c then Some c else None) else None
  in
  line |> String.to_list |> List.filter_mapi ~f
;;

let get_piles_exn =
  List.map ~f:get_row_char_opts
  %> List.transpose_exn
  %> Array.of_list_map ~f:List.(filter_map ~f:id %> rev %> Stack.of_list)
;;

let read_puzzle_exn filename =
  let rec aux acc ic =
    match In_channel.input_line ic with
    | Some line when line |> String.lstrip |> String.is_prefix ~prefix:"[" ->
      aux (line :: acc) ic
    | Some _ -> acc
    | None -> failwith "Early EOF"
  in
  let ic = In_channel.create filename in
  let crate_lines = aux [] ic in
  get_piles_exn crate_lines, get_moves ic
;;

let pop_n ~n ~rev stack =
  let rec aux n acc =
    if n <> 0
    then (
      let popped = Stack.pop_exn stack in
      aux (n - 1) (popped :: acc))
    else acc
  in
  let popped = aux n [] in
  if rev then List.rev popped else popped
;;

let rec stack_push_list rem stack =
  match rem with
  | [] -> ()
  | h :: t ->
    Stack.push stack h;
    stack_push_list t stack
;;

let solve_puzzle_exn ~rev filename =
  let piles, move_stream = read_puzzle_exn filename in
  let f { n; src; dst } =
    let popped = pop_n ~n ~rev piles.(src) in
    stack_push_list popped piles.(dst)
  in
  Sequence.iter ~f move_stream;
  piles
  |> Array.map ~f:Stack.top
  |> List.of_array
  |> List.filter_map ~f:id
  |> String.of_char_list
;;

let part1 = solve_puzzle_exn ~rev:true
let part2 = solve_puzzle_exn ~rev:false
