open Core
open Stdio

let ( %> ) g f x = x |> g |> f

type t =
  { items : int list
  ; operation : operation_t
  ; test : test_t
  }
[@@deriving sexp]

and operation_t =
  | Multiply of int
  | Add of int
  | Square

and test_t =
  { denominator : int
  ; true_target : int
  ; false_target : int
  }

let strip_if_not_empty s =
  if String.is_empty s then None else Option.return @@ String.strip s
;;

let line_chunks ?(sep = ':') line =
  String.split line ~on:sep |> List.filter_map ~f:strip_if_not_empty
;;

let tail_opt ~prefix = function
  | hd :: tl :: _ when String.equal hd prefix -> Some tl
  | _ -> None
;;

let ( let* ) opt f = Option.bind opt ~f

let parse_monkey_line line =
  let* _ = tail_opt (line_chunks ~sep:' ' line) ~prefix:"Monkey" in
  Some ()
;;

let parse_starting_line line =
  let* tl = tail_opt (line_chunks line) ~prefix:"Starting items" in
  String.split_on_chars tl ~on:[ ' '; ',' ]
  |> List.filter ~f:(String.is_empty %> not)
  |> List.filter_map ~f:Int.of_string_opt
  |> Option.return
;;

let parse_operation_line line =
  let* tl = tail_opt (line_chunks line) ~prefix:"Operation" in
  match String.split tl ~on:' ' with
  | "new" :: "=" :: "old" :: _ :: "old" :: _ -> Some Square
  | "new" :: "=" :: "old" :: operator :: operand :: _ ->
    (match operator, Int.of_string_opt operand with
     | "*", Some operand -> Some (Multiply operand)
     | "+", Some operand -> Some (Add operand)
     | _ -> None)
  | _ -> None
;;

let parse_pred_line line =
  let* tl =
    match line_chunks line with
    | "Test" :: tl :: _ -> Some tl
    | _ -> None
  in
  match String.split tl ~on:' ' with
  | "divisible" :: "by" :: denominator :: _ -> Int.of_string_opt denominator
  | _ -> None
;;

let parse_branch_line ~branch line =
  let* tl =
    match line_chunks line with
    | "If true" :: tl :: _ when Bool.(branch = true) -> Some tl
    | "If false" :: tl :: _ when Bool.(branch = false) -> Some tl
    | _ -> None
  in
  match String.split tl ~on:' ' with
  | "throw" :: "to" :: "monkey" :: target :: _ -> Int.of_string_opt target
  | _ -> None
;;

let of_string_list lines =
  match lines with
  | monkey :: starting :: operation :: pred :: true_line :: false_line :: _ ->
    let* () = parse_monkey_line monkey in
    let* items = parse_starting_line starting in
    let* operation = parse_operation_line operation in
    let* denominator = parse_pred_line pred in
    let* true_target = parse_branch_line true_line ~branch:true in
    let* false_target = parse_branch_line false_line ~branch:false in
    Some { items; operation; test = { denominator; true_target; false_target } }
  | _ -> None
;;

let seq_of_string_list_seq seq =
  let f seq =
    let* lines, seq = Sequence.next seq in
    let* t = of_string_list lines in
    Some (t, seq)
  in
  Sequence.unfold ~init:seq ~f
;;

let seq_of_string_seq seq =
  seq
  |> Sequence.group ~break:(fun _ b -> String.is_empty b)
  |> Sequence.map ~f:(List.filter ~f:(String.is_empty %> not))
  |> seq_of_string_list_seq
;;

let array_of_string_seq = seq_of_string_seq %> Sequence.to_array

let array_of_filename filename =
  let f ic =
    Sequence.unfold ~init:ic ~f:(fun ic ->
      In_channel.input_line ic |> Option.map ~f:(fun line -> String.strip line, ic))
    |> seq_of_string_seq
    |> Sequence.to_array
  in
  In_channel.with_file filename ~f
;;

let read_puzzle = array_of_filename
