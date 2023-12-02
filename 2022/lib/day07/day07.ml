open Core
open Tree
open Solve

let part1 filename =
  build_sized_tree filename
  |> get_trees_lte ~size:100000
  |> Sequence.fold ~init:0 ~f:(fun acc tree -> acc + tree.dirsize)
;;

let part2 filename =
  let tree = build_sized_tree filename in
  let needed_size = tree.dirsize - 40000000 in
  if needed_size <= 0
  then 0
  else
    tree
    |> get_trees_gte ~size:needed_size
    |> Sequence.map ~f:(fun tree -> tree.dirsize)
    |> Sequence.min_elt ~compare:Int.compare
    |> Option.value ~default:(-1)
;;
