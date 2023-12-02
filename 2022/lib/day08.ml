open Core
open Stdio
module Step = Sequence.Step

let id x = x
let ( %> ) g f x = x |> g |> f

let read_puzzle filename =
  let unfold_line_f ic =
    match In_channel.input_line ic with
    | Some line ->
      let array =
        line
        |> String.to_list
        |> List.map ~f:(fun c -> Char.to_int c - 48)
        |> Array.of_list
      in
      Some (array, ic)
    | None -> None
  in
  let f ic = Sequence.unfold ~init:ic ~f:unfold_line_f |> Sequence.to_array in
  In_channel.with_file ~f filename
;;

let rec range_excl lo hi =
  if Int.(lo < hi)
  then List.init (hi - lo) ~f:(fun i -> i + lo)
  else range_excl hi lo |> List.rev
;;

type direction_t =
  | Right
  | Up
  | Left
  | Down

let max_mapper matrix i j ~nrows:_ ~ncols:_ ~acc:axis_max ~direction:_ =
  let cur_height = matrix.(i).(j) in
  if cur_height > !axis_max
  then (
    axis_max := cur_height;
    matrix.(i).(j) <- 1)
  else matrix.(i).(j) <- 0
;;

let dist_mapper matrix i j ~nrows ~ncols ~acc:_ ~direction =
  let height = matrix.(i).(j) in
  let rec aux matrix i j ~height ~acc ~direction =
    match direction with
    | Right ->
      if Int.(j + 1 >= nrows)
      then acc
      else if matrix.(i).(j + 1) >= height
      then acc + 1
      else aux matrix i (j + 1) ~height ~acc:(acc + 1) ~direction
    | Left ->
      if Int.(j - 1 < 0)
      then acc
      else if matrix.(i).(j - 1) >= height
      then acc + 1
      else aux matrix i (j - 1) ~height ~acc:(acc + 1) ~direction
    | Down ->
      if Int.(i + 1 >= ncols)
      then acc
      else if matrix.(i + 1).(j) >= height
      then acc + 1
      else aux matrix (i + 1) j ~height ~acc:(acc + 1) ~direction
    | Up ->
      if Int.(i - 1 < 0)
      then acc
      else if matrix.(i - 1).(j) >= height
      then acc + 1
      else aux matrix (i - 1) j ~height ~acc:(acc + 1) ~direction
  in
  matrix.(i).(j) <- aux matrix i j ~height ~acc:0 ~direction
;;

let calculate_visibility matrix ~nrows ~ncols ~direction ~mapper =
  let i_range, j_range =
    match direction with
    | Right -> range_excl 0 nrows, range_excl 0 ncols
    | Left -> range_excl 0 nrows, range_excl ncols 0
    | Down -> range_excl 0 nrows, range_excl 0 ncols
    | Up -> range_excl nrows 0, range_excl 0 ncols
  in
  (match direction with
   | Left | Right ->
     List.iter i_range ~f:(fun i ->
       let acc = ref (-1) in
       List.iter j_range ~f:(fun j -> mapper matrix i j ~nrows ~ncols ~acc ~direction))
   | Up | Down ->
     List.iter j_range ~f:(fun j ->
       let acc = ref (-1) in
       List.iter i_range ~f:(fun i -> mapper matrix i j ~nrows ~ncols ~acc ~direction)));
  matrix
;;

let elementwise_or left right =
  Array.mapi left ~f:(fun i row ->
    let other_row = right.(i) in
    Array.zip_exn row other_row
    |> Array.map ~f:(fun (a, b) -> if Int.(a = 1 || b = 1) then 1 else 0))
;;

let elementwise_mult left right =
  Array.mapi left ~f:(fun i row ->
    let other_row = right.(i) in
    Array.zip_exn row other_row |> Array.map ~f:(fun (a, b) -> a * b))
;;

let sum_visible = Array.sum (module Int) ~f:(Array.sum (module Int) ~f:id)
let value_exn x = Option.value_exn x

let max_visibility =
  Array.map ~f:(Array.max_elt ~compare:Int.compare %> value_exn)
  %> Array.max_elt ~compare:Int.compare
  %> value_exn
;;

let solve filename ~mapper ~reducer ~collector =
  let matrix = read_puzzle filename in
  [ Right; Left; Down; Up ]
  |> List.map ~f:(fun direction ->
    let direction_matrix = Array.copy_matrix matrix in
    let ncols = Array.length direction_matrix.(0) in
    let nrows = Array.length direction_matrix in
    direction_matrix |> calculate_visibility ~nrows ~ncols ~mapper ~direction)
  |> List.reduce_exn ~f:reducer
  |> collector
;;

let part1 = solve ~mapper:max_mapper ~reducer:elementwise_or ~collector:sum_visible
let part2 = solve ~mapper:dist_mapper ~reducer:elementwise_mult ~collector:max_visibility
