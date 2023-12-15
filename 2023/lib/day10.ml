open Core
open Stdio
open Util

module Coord = struct
  type t = int * int [@@deriving sexp, compare, hash]
end

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp, equal]

  let move t (i, j) =
    match t with
    | Up -> i - 1, j
    | Down -> i + 1, j
    | Left -> i, j - 1
    | Right -> i, j + 1
  ;;
end

let is_in_bounds i j ~nrows ~ncols = i >= 0 && i < nrows && j >= 0 && j < ncols

module Tile = struct
  type t =
    | Vert_pipe
    | Horz_pipe
    | L_bend
    | J_bend
    | Seven_bend
    | F_bend
    | Ground
  [@@deriving sexp, equal]

  let is_ground = equal Ground
  let is_pipe = is_ground %> not

  (* The trick is that you can choose F and 7 too:
     Run across ...L-J... and that cancels out,
     but run across ...L-7... or ...F-J... and it doesn't! *)
  let is_horz_separator = function
    | Vert_pipe | L_bend | J_bend -> true
    | _ -> false
  ;;

  let connects_to t dir =
    let open Direction in
    match t with
    | Vert_pipe -> equal dir Up || equal dir Down
    | Horz_pipe -> equal dir Left || equal dir Right
    | L_bend -> equal dir Up || equal dir Right
    | J_bend -> equal dir Up || equal dir Left
    | Seven_bend -> equal dir Down || equal dir Left
    | F_bend -> equal dir Down || equal dir Right
    | Ground -> false
  ;;

  let rel_connections = function
    | Vert_pipe -> [ Direction.Up; Down ]
    | Horz_pipe -> [ Left; Right ]
    | L_bend -> [ Up; Right ]
    | J_bend -> [ Up; Left ]
    | Seven_bend -> [ Down; Left ]
    | F_bend -> [ Down; Right ]
    | Ground -> []
  ;;

  let naive_succs grid (i, j) =
    let nrows = Array.length grid in
    let ncols = Array.length grid.(0) in
    rel_connections grid.(i).(j)
    |> List.map ~f:(fun dir -> Direction.move dir (i, j))
    |> List.filter ~f:(fun (i', j') -> is_in_bounds i' j' ~nrows ~ncols)
  ;;

  let connected_coords grid (i, j) =
    let naive = naive_succs grid (i, j) in
    naive
    |> List.filter ~f:(fun (ni, nj) ->
      naive_succs grid (ni, nj) |> List.exists ~f:(fun (i', j') -> i = i' && j = j'))
  ;;

  let deduce grid (i, j) =
    let nrows = Array.length grid in
    let ncols = Array.length grid.(0) in
    let connects_to_bounded i j dir =
      if is_in_bounds i j ~nrows ~ncols then connects_to grid.(i).(j) dir else false
    in
    let c_up = connects_to_bounded (i - 1) j Down in
    let c_down = connects_to_bounded (i + 1) j Up in
    let c_left = connects_to_bounded i (j - 1) Right in
    let c_right = connects_to_bounded i (j + 1) Left in
    match c_up, c_down, c_left, c_right with
    | true, true, false, false -> Vert_pipe
    | false, false, true, true -> Horz_pipe
    | true, false, false, true -> L_bend
    | true, false, true, false -> J_bend
    | false, true, true, false -> Seven_bend
    | false, true, false, true -> F_bend
    | false, false, false, false -> Ground
    | _ -> failwith "whoops!"
  ;;
end

module Parse = struct
  let either_of_char c =
    match c with
    | '|' -> First Tile.Vert_pipe
    | '-' -> First Horz_pipe
    | 'L' -> First L_bend
    | 'J' -> First J_bend
    | '7' -> First Seven_bend
    | 'F' -> First F_bend
    | '.' -> First Ground
    | 'S' -> Second ()
    | _ -> failwith "invalid input!"
  ;;

  let row_of_string s = String.to_array s |> Array.map ~f:either_of_char
end

let find_loop grid init =
  let visited = Hash_set.create (module Coord) in
  let f ((i, j) as coords) =
    if Hash_set.mem visited coords
    then None
    else (
      Hash_set.add visited coords;
      Tile.connected_coords grid (i, j)
      |> List.filter ~f:(fun ij -> not @@ Hash_set.mem visited ij)
      |> List.hd
      |> Option.map ~f:(fun x -> x, x))
  in
  Sequence.of_list [ Sequence.return init; Sequence.unfold ~init ~f ] |> Sequence.concat
;;

let read_puzzle filename =
  let either_grid =
    In_channel.read_lines filename |> List.map ~f:Parse.row_of_string |> Array.of_list
  in
  let si, sj =
    Array.find_mapi_exn either_grid ~f:(fun i row ->
      Array.find_mapi row ~f:(fun j ele ->
        if Either.is_second ele then Some (i, j) else None))
  in
  let grid =
    Array.map either_grid ~f:(fun row ->
      Array.map row ~f:(function
        | First tile -> tile
        | Second _ -> Tile.Ground))
  in
  let start_tile = Tile.deduce grid (si, sj) in
  grid.(si).(sj) <- start_tile;
  grid, (si, sj)
;;

let print_grid grid =
  Array.iter grid ~f:(fun row ->
    Array.iter row ~f:(fun ele -> if ele then printf "%s" "X" else printf "%s" ".");
    printf "\n");
  printf "\n"
;;

let part1 filename =
  let grid, (si, sj) = read_puzzle filename in
  let loop_list = find_loop grid (si, sj) |> Sequence.to_list in
  List.length loop_list / 2 |> sprintf !"%{sexp:((int))}\n"
;;

let part2 filename =
  let tile_grid, (si, sj) = read_puzzle filename in
  let loop_seq = find_loop tile_grid (si, sj) in
  let nrows, ncols = Array.(length tile_grid, length tile_grid.(0)) in
  let loop_grid = Array.make_matrix false ~dimx:nrows ~dimy:ncols in
  Sequence.iter loop_seq ~f:(fun (i, j) -> loop_grid.(i).(j) <- true);
  let in_grid = Array.make_matrix true ~dimx:nrows ~dimy:ncols in
  let left_to_right, right_to_left, top_to_bottom =
    Sequence.(
      range 0 ncols, range (ncols - 1) 0 ~stride:(-1) ~stop:`inclusive, range 0 nrows)
  in
  Sequence.iter top_to_bottom ~f:(fun i ->
    List.iter [ left_to_right; right_to_left ] ~f:(fun through_row_iter ->
      let cur_in = ref false in
      Sequence.iter through_row_iter ~f:(fun j ->
        if Tile.is_horz_separator tile_grid.(i).(j) && loop_grid.(i).(j)
        then cur_in := not !cur_in;
        in_grid.(i).(j) <- in_grid.(i).(j) && !cur_in && (not @@ loop_grid.(i).(j)))));
  (* print_grid in_grid; *)
  Array.map in_grid ~f:(fun row -> Array.count row ~f:id)
  |> Array.sum (module Int) ~f:id
  |> sprintf !"%{sexp:((int))}\n"
;;
