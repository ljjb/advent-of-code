open Core

module Inner = struct
  type t = int * int [@@deriving sexp, compare, equal, hash]
end

include Inner
module Heap = Hash_heap.Make (Inner)

let in_bounds (i, j) ~nrows ~ncols = i >= 0 && i < nrows && j >= 0 && j < ncols
let manhattan (xi, xj) (yi, yj) = Int.(abs (xi - yi) + abs (xj - yj))
