open Core
open Stdio
open Util

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp, compare, hash, equal]

  let bounce_up = function
    | Up -> Right
    | Down -> Left
    | Left -> Down
    | Right -> Up
  ;;

  let bounce_down = function
    | Up -> Left
    | Down -> Right
    | Left -> Up
    | Right -> Down
  ;;

  let reverse = function
    | Up -> Down
    | Down -> Up
    | Left -> Right
    | Right -> Left
  ;;

  let is_horz = function
    | Up -> false
    | Down -> false
    | Left -> true
    | Right -> true
  ;;

  let is_vert = is_horz %> not
end

module Coord = struct
  type t = int * int [@@deriving sexp, compare, hash, equal]

  let move (i, j) = function
    | Direction.Up -> i - 1, j
    | Down -> i + 1, j
    | Left -> i, j - 1
    | Right -> i, j + 1
  ;;

  let in_bounds (i, j) ~nrows ~ncols = i >= 0 && i < ncols && j >= 0 && j < nrows
end

module Tile = struct
  type t =
    | Empty
    | Mirror_up
    | Mirror_down
    | Splitter_horz
    | Splitter_vert
  [@@deriving sexp, compare, equal]

  let of_char = function
    | '.' -> Empty
    | '/' -> Mirror_up
    | '\\' -> Mirror_down
    | '-' -> Splitter_horz
    | '|' -> Splitter_vert
    | _ -> failwith "invalid tile!"
  ;;

  let to_char = function
    | Empty -> '.'
    | Mirror_up -> '/'
    | Mirror_down -> '\\'
    | Splitter_horz -> '-'
    | Splitter_vert -> '|'
  ;;
end

module Beam_head = struct
  type t =
    { pos : Coord.t
    ; dir : Direction.t
    }
  [@@deriving sexp, compare, hash, equal]

  let get_pos t = t.pos
  let move t dir = { pos = Coord.move t.pos dir; dir }
  let move_self t = move t t.dir

  let interact_with t = function
    | Tile.Mirror_up -> [ Direction.bounce_up t.dir |> move t ]
    | Mirror_down -> [ Direction.bounce_down t.dir |> move t ]
    | Splitter_horz when Direction.is_vert t.dir ->
      [ move t Direction.Left; move t Direction.Right ]
    | Splitter_vert when Direction.is_horz t.dir ->
      [ move t Direction.Up; move t Direction.Down ]
    | _ -> [ move_self t ]
  ;;
end

let grid_to_string =
  Array.map ~f:(Array.map ~f:Tile.to_char %> String.of_array)
  %> String.concat_array ~sep:"\n"
;;

let visited_to_string visited ~nrows ~ncols =
  let grid = Array.make_matrix '.' ~dimx:nrows ~dimy:ncols in
  Hash_set.iter visited ~f:(fun node ->
    let i, j = node.Beam_head.pos in
    grid.(i).(j) <- '#');
  Array.map grid ~f:String.of_array |> String.concat_array ~sep:"\n"
;;

let run_simulation grid start =
  let nrows, ncols = Array.(length grid, length grid.(0)) in
  let in_bounds = Coord.in_bounds ~nrows ~ncols in
  let visited = Hash_set.create (module Beam_head) in
  let q = Queue.create () in
  Queue.enqueue q start;
  while not @@ Queue.is_empty q do
    let node = Queue.dequeue_exn q in
    if not @@ Hash_set.mem visited node
    then (
      Hash_set.add visited node;
      let i, j = node.pos in
      let succs =
        Beam_head.interact_with node grid.(i).(j)
        |> List.filter ~f:(Beam_head.get_pos %> in_bounds)
      in
      Queue.enqueue_all q succs)
  done;
  (* printf !"\n%s\n" (visited_to_string visited ~nrows ~ncols); *)
  Hash_set.to_list visited
  |> List.map ~f:Beam_head.get_pos
  |> Hash_set.of_list (module Coord)
  |> Hash_set.length
;;

let get_top_left_corner _ = [ Beam_head.{ pos = 0, 0; dir = Right } ]

let get_all_edges grid =
  let nrows, ncols = Array.(length grid, length grid.(0)) in
  let horz, vert = Sequence.(range 0 ncols, range 0 ncols) in
  let top = Sequence.map horz ~f:(fun j -> Beam_head.{ pos = 0, j; dir = Down }) in
  let bot = Sequence.map horz ~f:(fun j -> Beam_head.{ pos = nrows - 1, j; dir = Up }) in
  let left = Sequence.map vert ~f:(fun i -> Beam_head.{ pos = i, 0; dir = Right }) in
  let right =
    Sequence.map vert ~f:(fun i -> Beam_head.{ pos = i, ncols - 1; dir = Left })
  in
  List.concat_map [ top; bot; left; right ] ~f:Sequence.to_list
;;

let solve filename ~starts_f =
  let grid =
    In_channel.read_lines filename
    |> Array.of_list_map ~f:(String.to_array %> Array.map ~f:Tile.of_char)
  in
  let starts = starts_f grid in
  List.map starts ~f:(fun start -> run_simulation grid start)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
  |> sprintf !"%d"
;;

let part1 = solve ~starts_f:get_top_left_corner
let part2 = solve ~starts_f:get_all_edges
