open Core
open Stdio
open Util

let array_of_lines lines =
  List.filter lines ~f:(String.is_empty %> not)
  |> Array.of_list_map ~f:(fun line ->
    String.to_array line
    |> Array.map ~f:(fun c ->
      match c with
      | '#' -> true
      | '.' -> false
      | _ -> failwith "invalid grid input!"))
;;

let dim_slice arr i_or_j ~dim =
  let nrows, ncols = Array.(length arr, length arr.(0)) in
  let n, get_slice_nth =
    match dim with
    | `Row -> ncols, fun j -> arr.(i_or_j).(j)
    | `Col -> nrows, fun i -> arr.(i).(i_or_j)
  in
  Sequence.range 0 n |> Sequence.map ~f:get_slice_nth
;;

let bools_to_int bs =
  Sequence.foldi bs ~init:0 ~f:(fun i acc b -> acc + (Bool.to_int b * (1 lsl i)))
;;

let reduce_dims arr =
  let nrows, ncols = Array.(length arr, length arr.(0)) in
  ( List.init nrows ~f:(fun i -> dim_slice arr i ~dim:`Row |> bools_to_int)
  , List.init ncols ~f:(fun j -> dim_slice arr j ~dim:`Col |> bools_to_int) )
;;

(* Operates on pre-expanded array to avoid reallocating m*n times in part 2 *)
let longest_pal_expanded ~arr ~pals ~aug_len ~exclude_i =
  let rec inner_loop i r =
    let lo, hi = i - r, i + r in
    if lo >= 0 && hi < aug_len && arr.(lo) = arr.(hi)
    then (
      pals.(i) <- pals.(i) + 1;
      inner_loop i (r + 1))
  in
  let rec outer_loop i =
    if i < aug_len
    then (
      inner_loop i 1;
      outer_loop (i + 2))
  in
  outer_loop 0;
  let center_i, _ =
    Array.filter_mapi pals ~f:(fun i ele ->
      (* These boundary conditions were not at all clear to me from the text. The palindrome
         has to be bounded by one edge of the string! *)
      if ele > 0 && (i - ele = 0 || i + ele = aug_len - 1) then Some (i, ele) else None)
    |> Array.fold ~init:(0, 0) ~f:(fun (acc_i, acc_ele) (i, ele) ->
      if ele > acc_ele && not (i = exclude_i) then i, ele else acc_i, acc_ele)
  in
  center_i
;;

let longest_pal l =
  let arr = List.(concat [ [ -1 ]; intersperse l ~sep:(-1); [ -1 ] ]) |> Array.of_list in
  let aug_len = Array.length arr in
  let pals = Array.init aug_len ~f:(const 0) in
  let center_aug_i = longest_pal_expanded ~arr ~pals ~aug_len ~exclude_i:(-1) in
  center_aug_i / 2
;;

(* Since we're representing rows/cols as bit vecs, we can
   smudge a mirror by flipping a bit *)
let flip_nth_bit x ~n = x lxor (1 lsl n)

(* Mutating iterator: array will be unchanged after it has been fully consumed; you
   can't just pipe this into Array.to_list *)
let every_flip_ith arr i ~num_bits =
  let open Sequence.Generator in
  let ( let* ) = ( >>= ) in
  let rec monad n =
    if n < num_bits
    then (
      let saved = arr.(i) in
      arr.(i) <- flip_nth_bit arr.(i) ~n;
      let* _ = yield arr in
      arr.(i) <- saved;
      monad (n + 1))
    else return ()
  in
  run (monad 0)
;;

let every_flip arr ~num_bits =
  let open Sequence.Generator in
  let ( let* ) = ( >>= ) in
  let len = Array.length arr in
  let rec monad i =
    if i < len
    then
      let* _ = yield @@ every_flip_ith arr i ~num_bits in
      monad (i + 2)
    else return ()
  in
  run (monad 1) |> Sequence.concat
;;

let longest_pal_smudged l ~num_bits =
  let arr = List.(concat [ [ -1 ]; intersperse l ~sep:(-1); [ -1 ] ]) |> Array.of_list in
  let aug_len = Array.length arr in
  let pals = Array.init aug_len ~f:(const 0) in
  let orig_center_i = longest_pal_expanded ~arr ~pals ~aug_len ~exclude_i:(-1) in
  let center_i =
    every_flip arr ~num_bits
    |> Sequence.find_map ~f:(fun smudged ->
      (* Note we could use arr instead of smudged here because we're
         using a slippery arr-mutating sequence *)
      Array.fill pals ~pos:0 ~len:aug_len 0;
      let center_i =
        longest_pal_expanded ~arr:smudged ~pals ~aug_len ~exclude_i:orig_center_i
      in
      if center_i > 0 then Some center_i else None)
    |> Option.value ~default:0
  in
  center_i / 2
;;

let score (rows, cols) = if rows > cols then 100 * rows else cols

let solve ~longest_pal_f filename =
  In_channel.read_lines filename
  |> List.group ~break:(fun _ b -> String.is_empty b)
  |> List.map ~f:(array_of_lines %> longest_pal_f)
  |> List.fold ~init:0 ~f:(fun acc rows_and_cols -> acc + score rows_and_cols)
  |> sprintf !"%{sexp:(int)}"
;;

let part1 = solve ~longest_pal_f:(reduce_dims %> Tuple2.map ~f:longest_pal)

let part2 =
  let longest_pal_f arr =
    let nrows, ncols = Array.(length arr, length arr.(0)) in
    let rows, cols = reduce_dims arr in
    longest_pal_smudged rows ~num_bits:ncols, longest_pal_smudged cols ~num_bits:nrows
  in
  solve ~longest_pal_f
;;
