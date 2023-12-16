open Core
open Stdio

let to_unexpanded lines =
  List.folding_map lines ~init:1 ~f:(fun acc line ->
    String.to_array line
    |> Array.fold_map ~init:acc ~f:(fun acc ele ->
      if Char.equal ele '#' then acc + 1, acc else acc, 0))
  |> Array.of_list
;;

let ranges grid =
  let nrows, ncols = Array.(length grid, length grid.(0)) in
  Sequence.(range 0 ncols, range 0 nrows)
;;

let get_vastness grid =
  let horz_range, vert_range = ranges grid in
  let empty_rows =
    Sequence.fold
      vert_range
      ~init:(Set.empty (module Int))
      ~f:(fun acc i ->
        if Array.for_all grid.(i) ~f:(Int.equal 0) then Set.add acc i else acc)
  in
  let empty_cols =
    Sequence.fold
      horz_range
      ~init:(Set.empty (module Int))
      ~f:(fun acc j ->
        if Sequence.for_all vert_range ~f:(fun i -> grid.(i).(j) = 0)
        then Set.add acc j
        else acc)
  in
  empty_rows, empty_cols
;;

let get_galaxies grid =
  let horz_range, vert_range = ranges grid in
  Sequence.cartesian_product horz_range vert_range
  |> Sequence.filter ~f:(fun (i, j) -> not (grid.(i).(j) = 0))
  (* Crucial to performance because of combinations calculation below *)
  |> Sequence.force_eagerly
;;

let combinations seq =
  Sequence.folding_map seq ~init:seq ~f:(fun acc a ->
    let acc = Sequence.drop acc 1 in
    acc, Sequence.map acc ~f:(fun b -> a, b))
  |> Sequence.concat
;;

let expanded_manhattan (ai, aj) (bi, bj) ~empty_rows ~empty_cols ~added =
  let ai, bi = if ai > bi then bi, ai else ai, bi in
  let aj, bj = if aj > bj then bj, aj else aj, bj in
  let row_delta =
    Set.to_sequence empty_rows ~greater_or_equal_to:ai ~less_or_equal_to:bi
    |> Sequence.length
  in
  let col_delta =
    Set.to_sequence empty_cols ~greater_or_equal_to:aj ~less_or_equal_to:bj
    |> Sequence.length
  in
  bi - ai + (bj - aj) + ((row_delta + col_delta) * added)
;;

let solve ~expansion filename =
  let added = expansion - 1 in
  let grid = In_channel.read_lines filename |> to_unexpanded in
  let empty_rows, empty_cols = get_vastness grid in
  let all_pairs = get_galaxies grid |> combinations in
  Sequence.sum
    (module Int)
    all_pairs
    ~f:(fun (a, b) -> expanded_manhattan a b ~empty_rows ~empty_cols ~added)
  |> sprintf !"%{sexp:(int)}"
;;

let part1 = solve ~expansion:2
let part2 = solve ~expansion:100000
