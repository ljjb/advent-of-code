open Core

type t =
  | Up
  | Down
  | Left
  | Right
[@@deriving sexp, compare, hash, equal]

let rel_coord = function
  | Up -> -1, 0
  | Down -> 1, 0
  | Left -> 0, -1
  | Right -> 0, 1
;;

let all = [ Up; Down; Left; Right ]
let all_with_rel = all |> List.map ~f:(fun dir -> dir, rel_coord dir)

let opposite = function
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left
;;

let are_opposites x y = opposite x |> equal y
