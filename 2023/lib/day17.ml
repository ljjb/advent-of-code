open Core
open Stdio
open Util

module Coord = struct
  module Inner = struct
    type t = int * int [@@deriving sexp, compare, equal, hash]
  end

  include Inner
  module Heap = Hash_heap.Make (Inner)

  let in_bounds (i, j) ~nrows ~ncols = i >= 0 && i < nrows && j >= 0 && j < ncols
  let manhattan (xi, xj) (yi, yj) = Int.(abs (xi - yi) + abs (xj - yj))
end

module Direction = struct
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
end

module Node = struct
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

  let start_of_coords pos =
    (* Bearing doesn't matter because of consecutive being 0 *)
    { pos; bearing = Right; consecutive = 0 }
  ;;

  let all_possible_of_coords pos =
    List.cartesian_product Direction.all [ 1; 2; 3 ]
    |> List.map ~f:(fun (bearing, consecutive) -> { pos; bearing; consecutive })
  ;;

  let succs t ~nrows ~ncols =
    List.filter_map Direction.all_with_rel ~f:(fun (dir, (di, dj)) ->
      let i, j = t.pos in
      let i, j = i + di, j + dj in
      if Direction.are_opposites dir t.bearing
         || (not @@ Coord.in_bounds (i, j) ~nrows ~ncols)
      then None
      else (
        let consecutive =
          if Direction.equal t.bearing dir then t.consecutive + 1 else 1
        in
        if consecutive > 3 then None else Some { pos = i, j; bearing = dir; consecutive }))
  ;;
end

let digit_to_int_opt c =
  if Char.is_digit c then Some (Char.to_int c - Char.to_int '0') else None
;;

let dijkstra ~heat_loss ~start_node ~end_node =
  let nrows = Array.length heat_loss in
  let ncols = Array.length heat_loss.(0) in
  let g_score = Hashtbl.create (module Node) in
  let get_g_score node =
    Hashtbl.find g_score node |> Option.value ~default:Int.max_value
  in
  let open_set =
    Node.Heap.create (fun x y ->
      let f_x = get_g_score x in
      let f_y = get_g_score y in
      Int.compare f_x f_y)
  in
  Hashtbl.set g_score ~key:start_node ~data:0;
  ignore @@ Node.Heap.push open_set ~key:start_node ~data:start_node;
  let rec loop () =
    match Node.Heap.pop open_set with
    | None -> None
    | Some cur_node ->
      if Node.equal cur_node end_node
      then Some (get_g_score cur_node)
      else (
        Node.succs cur_node ~nrows ~ncols
        |> List.iter ~f:(fun ({ pos = ni, nj; _ } as nbor) ->
          let tent_g = get_g_score cur_node + heat_loss.(ni).(nj) in
          if tent_g < get_g_score nbor
          then (
            Hashtbl.set g_score ~key:nbor ~data:tent_g;
            ignore @@ Node.Heap.find_pop open_set nbor;
            ignore @@ Node.Heap.push open_set ~key:nbor ~data:nbor));
        loop ())
  in
  loop ()
;;

let read_puzzle =
  In_channel.read_lines
  %> Array.of_list_map
       ~f:(String.to_list %> List.filter_map ~f:digit_to_int_opt %> Array.of_list)
;;

let part1 filename =
  let heat_loss = read_puzzle filename in
  let nrows = Array.length heat_loss in
  let ncols = Array.length heat_loss.(0) in
  let start_node = Node.start_of_coords (0, 0) in
  let end_nodes = Node.all_possible_of_coords (nrows - 1, ncols - 1) in
  List.filter_map end_nodes ~f:(fun end_node ->
    let out = dijkstra ~heat_loss ~start_node ~end_node in
    (* printf *)
    (*   !"With end node of %{sexp:(Node.t)}, best is %{sexp:(int option)}\n" *)
    (*   end_node *)
    (*   out; *)
    out)
  |> List.min_elt ~compare:Int.compare
  |> sprintf !"%{sexp:(int option)}"
;;
