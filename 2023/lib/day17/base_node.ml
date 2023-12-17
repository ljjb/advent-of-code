open Core

module Inner = struct
  type t =
    { pos : Coord.t
    ; bearing : Direction.t
    ; consecutive : int
    }
  [@@deriving sexp, compare, equal, hash]
end

include Inner
module Heap = Hash_heap.Make (Inner)

let get_pos { pos; _ } = pos
let h x y = Coord.manhattan x.pos y.pos

let start_of_coords pos =
  (* Bearing doesn't matter because of consecutive being 0 *)
  { pos; bearing = Right; consecutive = 0 }
;;
