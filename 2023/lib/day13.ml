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
  Sequence.foldi bs ~init:0 ~f:(fun i acc b -> acc + (Bool.to_int b * Int.pow 2 i))
;;

let reduce_dims arr =
  let nrows, ncols = Array.(length arr, length arr.(0)) in
  ( List.init nrows ~f:(fun i -> dim_slice arr i ~dim:`Row |> bools_to_int)
  , List.init ncols ~f:(fun j -> dim_slice arr j ~dim:`Col |> bools_to_int) )
;;

let longest_palindrome l =
  let arr = List.(concat [ [ -1 ]; intersperse l ~sep:(-1); [ -1 ] ]) |> Array.of_list in
  let aug_n = Array.length arr in
  let pals = Array.init aug_n ~f:(const 0) in
  let rec inner_loop i r =
    let lo, hi = i - r, i + r in
    if lo >= 0 && hi < aug_n && arr.(lo) = arr.(hi)
    then (
      pals.(i) <- pals.(i) + 1;
      inner_loop i (r + 1))
  in
  let rec outer_loop i =
    if i < aug_n
    then (
      inner_loop i 1;
      outer_loop (i + 2))
  in
  outer_loop 0;
  let center_i, _ =
    Array.filter_mapi pals ~f:(fun i ele ->
      (* These boundary conditions were not at all clear to me from the text. The palindrome
         has to be bounded by one edge of the string! *)
      if ele > 0 && (i - ele = 0 || i + ele = aug_n - 1) then Some (i, ele) else None)
    |> Array.fold ~init:(0, 0) ~f:(fun (acc_i, acc_ele) (i, ele) ->
      if ele > acc_ele then i, ele else acc_i, acc_ele)
  in
  Array.partitioni_tf arr ~f:(fun i _ -> i < center_i)
  |> fst
  |> Array.filter ~f:(fun ele -> ele >= 0)
  |> Array.length
;;

let score (rows, cols) = if rows > cols then 100 * rows else cols

let part1 filename =
  In_channel.read_lines filename
  |> List.group ~break:(fun _ b -> String.is_empty b)
  |> List.map ~f:(array_of_lines %> reduce_dims)
  |> List.map ~f:(fun (rows, cols) -> longest_palindrome rows, longest_palindrome cols)
  |> List.fold ~init:0 ~f:(fun acc rows_and_cols -> acc + score rows_and_cols)
  |> sprintf !"%{sexp:(int)}"
;;
