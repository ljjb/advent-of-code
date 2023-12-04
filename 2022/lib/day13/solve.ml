open Base
open Stdio

let id x = x

let rec properly_ordered l r =
  let open Puzzle in
  match l, r with
  | Single l, Single r -> if Int.(l = r) then None else Some Int.(l < r)
  | (Single _ as l), (Many _ as r) -> properly_ordered (Many [ l ]) r
  | (Many _ as l), (Single _ as r) -> properly_ordered l (Many [ r ])
  | Many l, Many r ->
    (match l, r with
     | [], [] -> None
     | _, [] -> Some false
     | [], _ -> Some true
     | lhd :: ltl, rhd :: rtl ->
       (match properly_ordered lhd rhd with
        | Some _ as some -> some
        | None -> properly_ordered (Many ltl) (Many rtl)))
;;

let test_pair l r =
  let l = Puzzle.parse_puzzle_list l |> Result.ok |> Option.value_exn in
  let r = Puzzle.parse_puzzle_list r |> Result.ok |> Option.value_exn in
  match properly_ordered l r with
  | Some true -> true
  | _ -> false
;;

let%test "pair 1" = test_pair "[1,1,3,1,1]" "[1,1,5,1,1]"
let%test "pair 2" = test_pair "[[1],[2,3,4]]" "[[1],4]"
let%test "pair 3" = not @@ test_pair "[9]" "[[8,7,6]]"
let%test "pair 4" = test_pair "[[4,4],4,4]" "[[4,4],4,4,4]"
let%test "pair 5" = not @@ test_pair "[7,7,7,7]" "[7,7,7]"
let%test "pair 6" = test_pair "[]" "[3]"
let%test "pair 7" = not @@ test_pair "[[[]]]" "[[]]"

let%test "pair 8" =
  not @@ test_pair "[1,[2,[3,[4,[5,6,7]]]],8,9]" "[1,[2,[3,[4,[5,6,0]]]],8,9]"
;;

let part1 filename =
  let f ic =
    Puzzle.pair_seq_of_ic ic
    |> Sequence.mapi ~f:(fun i (left, right) -> i, left, right)
    |> Sequence.fold ~init:0 ~f:(fun acc (i, left, right) ->
      match properly_ordered left right with
      | Some true -> acc + i + 1
      | _ -> acc)
  in
  In_channel.with_file filename ~f
;;

let div_lo = Puzzle.(Many [ Many [ Single 2 ] ])
let div_hi = Puzzle.(Many [ Many [ Single 6 ] ])

let part2 filename =
  let f ic =
    let fsts, snds = Puzzle.pair_seq_of_ic ic |> Sequence.to_list |> List.unzip in
    List.concat_no_order [ fsts; snds; [ div_lo; div_hi ] ]
  in
  In_channel.with_file filename ~f
  |> List.sort ~compare:(fun l r ->
    match properly_ordered l r with
    | None -> 0
    | Some true -> -1
    | Some false -> 1)
  |> List.foldi ~init:1 ~f:(fun i acc ele ->
    if Puzzle.(equal ele div_lo || equal ele div_hi) then acc * (i + 1) else acc)
;;
