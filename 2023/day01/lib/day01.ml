open Core
open Stdio

let id x = x

let read_puzzle ic =
  let f ic =
    match In_channel.input_line ic with
    | Some line -> Some (line, ic)
    | None -> None
  in
  Sequence.unfold ~init:ic ~f
;;

let calibration int_line =
  let fold_f (left, _) ele =
    let left' =
      match left with
      | None -> Some ele
      | _ -> left
    in
    let right' = Some ele in
    left', right'
  in
  match List.fold int_line ~init:(None, None) ~f:fold_f with
  | Some left, Some right -> (10 * left) + right
  | _ -> 0
;;

let filter_part1 line =
  let f ele = if Char.is_digit ele then Some (Char.to_int ele - 48) else None in
  String.to_list line |> List.filter_map ~f
;;

let filter_part2 line =
  let init = Map.empty (module Int) in
  (* The 7even-8ight-9ine algorithm *)
  let subs =
    [ "one", '1'
    ; "two", '2'
    ; "three", '3'
    ; "four", '4'
    ; "five", '5'
    ; "six", '6'
    ; "seven", '7'
    ; "eight", '8'
    ; "nine", '9'
    ]
  in
  let map =
    List.fold subs ~init ~f:(fun acc (word, digit) ->
      String.substr_index_all line ~may_overlap:false ~pattern:word
      |> List.fold ~init:acc ~f:(fun acc idx -> Map.set acc ~key:idx ~data:digit))
  in
  String.mapi line ~f:(fun i c ->
    match Map.find map i with
    | None -> c
    | Some c -> c)
  |> filter_part1
;;

let solve filename ~filter =
  let f ic =
    read_puzzle ic
    |> Sequence.map ~f:filter
    |> Sequence.map ~f:calibration
    |> Sequence.sum (module Int) ~f:id
  in
  In_channel.with_file filename ~f
;;

let part1 = solve ~filter:filter_part1
let part2 = solve ~filter:filter_part2
