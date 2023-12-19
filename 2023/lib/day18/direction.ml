open Core

type t =
  | Up
  | Down
  | Left
  | Right
[@@deriving sexp, compare, equal, hash]

let of_char = function
  | 'U' | '3' -> Up
  | 'D' | '1' -> Down
  | 'L' | '2' -> Left
  | 'R' | '0' -> Right
  | _ -> failwith "invalid direction!"
;;
