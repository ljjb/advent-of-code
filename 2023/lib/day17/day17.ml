open Core
open Stdio
open Util

module type Puzzle_node = sig
  include module type of Base_node
  module Heap = Base_node.Heap

  val all_possible_of_coords : Coord.t -> t list
  val successors : t -> nrows:int -> ncols:int -> t list
  val cost : int array array -> t -> t -> int
end

module Part1 : Puzzle_node = struct
  include Base_node

  let all_possible_of_coords pos =
    List.cartesian_product Direction.all [ 1; 2; 3 ]
    |> List.map ~f:(fun (bearing, consecutive) -> { pos; bearing; consecutive })
  ;;

  let successors t ~nrows ~ncols =
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

  let cost heat_loss _ { pos = bi, bj; _ } = heat_loss.(bi).(bj)
end

module Part2 : Puzzle_node = struct
  include Base_node

  let all_possible_of_coords pos =
    List.cartesian_product [ Direction.Right; Direction.Down ] [ 4; 5; 6; 7; 8; 9; 10 ]
    |> List.map ~f:(fun (bearing, consecutive) -> { pos; bearing; consecutive })
  ;;

  let successors t ~nrows ~ncols =
    List.filter_map Direction.all_with_rel ~f:(fun (dir, (di, dj)) ->
      if Direction.are_opposites dir t.bearing
      then None
      else (
        let i, j = t.pos in
        if Direction.equal dir t.bearing
        then (
          let consecutive = t.consecutive + 1 in
          if consecutive > 10
          then None
          else (
            let i, j = i + di, j + dj in
            if Coord.in_bounds (i, j) ~nrows ~ncols
            then Some { pos = i, j; bearing = dir; consecutive }
            else None))
        else (
          let i, j = i + (4 * di), j + (4 * dj) in
          if Coord.in_bounds (i, j) ~nrows ~ncols
          then Some { pos = i, j; bearing = dir; consecutive = 4 }
          else None)))
  ;;

  let cost heat_loss { pos = ai, aj; _ } { pos = bi, bj; _ } =
    if ai = bi
    then (
      let stride = if aj < bj then 1 else -1 in
      Sequence.range aj bj ~stride ~start:`exclusive ~stop:`inclusive
      |> Sequence.sum (module Int) ~f:(fun j -> heat_loss.(ai).(j)))
    else if aj = bj
    then (
      let stride = if ai < bi then 1 else -1 in
      Sequence.range ai bi ~stride ~start:`exclusive ~stop:`inclusive
      |> Sequence.sum (module Int) ~f:(fun i -> heat_loss.(i).(aj)))
    else failwith "bad input to calculate_cost!"
  ;;
end

let a_star (module Node : Puzzle_node) ~heat_loss ~start ~goal =
  let nrows = Array.length heat_loss in
  let ncols = Array.length heat_loss.(0) in
  let g_score = Hashtbl.create (module Node) in
  let f_score = Hashtbl.create (module Node) in
  let get_g_score node =
    Hashtbl.find g_score node |> Option.value ~default:Int.max_value
  in
  let get_f_score node =
    Hashtbl.find g_score node |> Option.value ~default:Int.max_value
  in
  let open_set =
    Node.Heap.create (fun x y ->
      let f_x = get_f_score x in
      let f_y = get_f_score y in
      Int.compare f_x f_y)
  in
  Hashtbl.set g_score ~key:start ~data:0;
  ignore @@ Node.Heap.push open_set ~key:start ~data:start;
  let rec loop () =
    match Node.Heap.pop open_set with
    | None -> None
    | Some cur ->
      if Node.equal cur goal
      then Some (get_g_score cur)
      else (
        Node.successors cur ~nrows ~ncols
        |> List.iter ~f:(fun nbor ->
          let tent_g = get_g_score cur + Node.cost heat_loss cur nbor in
          if tent_g < get_g_score nbor
          then (
            Hashtbl.set g_score ~key:nbor ~data:tent_g;
            let tent_f = tent_g + Node.h nbor goal in
            if get_f_score nbor > tent_f then Hashtbl.set f_score ~key:nbor ~data:tent_f;
            ignore @@ Node.Heap.find_pop open_set nbor;
            ignore @@ Node.Heap.push open_set ~key:nbor ~data:nbor));
        loop ())
  in
  loop ()
;;

let read_puzzle =
  let digit_to_int_opt c =
    if Char.is_digit c then Some (Char.to_int c - Char.to_int '0') else None
  in
  In_channel.read_lines
  %> Array.of_list_map
       ~f:(String.to_list %> List.filter_map ~f:digit_to_int_opt %> Array.of_list)
;;

let solve (module Node : Puzzle_node) filename =
  let heat_loss = read_puzzle filename in
  let nrows = Array.length heat_loss in
  let ncols = Array.length heat_loss.(0) in
  let start = Node.start_of_coords (0, 0) in
  Node.all_possible_of_coords (nrows - 1, ncols - 1)
  |> List.filter_map ~f:(fun goal -> a_star (module Node) ~heat_loss ~start ~goal)
  |> List.min_elt ~compare:Int.compare
  |> sprintf !"%{sexp:(int option)}"
;;

let part1 = solve (module Part1)
let part2 = solve (module Part2)
