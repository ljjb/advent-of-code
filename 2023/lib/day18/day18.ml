open Core
open Stdio
module Iteration = Util.Iteration

module Coords = struct
  type t = int * int [@@deriving sexp, compare, equal, hash]

  let scale (i, j) ~factor = i * factor, j * factor

  let move (i, j) = function
    | Direction.Up -> i - 1, j
    | Down -> i + 1, j
    | Left -> i, j - 1
    | Right -> i, j + 1
  ;;

  let move_rel = move (0, 0)

  let move_n (i, j) dir n =
    let di, dj = move_rel dir |> scale ~factor:n in
    i + di, j + dj
  ;;

  let move_seq coords dir ~count =
    Sequence.unfold ~init:(coords, count) ~f:(fun (coords, count) ->
      if count = 0
      then None
      else (
        let new_coords = move coords dir in
        Some (new_coords, (new_coords, count - 1))))
  ;;

  let to_floats (i, j) = Float.(of_int i, of_int j)
  let cross_floats (ai, aj) (bi, bj) = (aj *. bi) -. (ai *. bj)
  let ( - ) (ai, aj) (bi, bj) = ai - bi, aj - bj
end

module Corner = struct
  type t =
    { coords : Coords.t
    ; dir_in : Direction.t
    ; dir_out : Direction.t
    }
  [@@deriving sexp, compare, equal]

  (* This is a trick to allow using the Shoelace formula even though
     our coordinates are in the "center" of tiles, making the area calculation
     slightly off. When we turn corners, we push the coordinates to the
     appropriate diagonal in a new coordinate system where the coordinates describe
     the corners of the tiles rather than the centers. To know which diagonal, we
     rely on knowing whether we're going clockwise or anticlockwise.

     For instance, say we're going right and turn down. If we're going clockwise
     then we know the actual boundary of the shape is on the northeast corner, which
     in the original (i, j) center-coordinate system is transformed to (i, j + 1)
     in the new corner-coordinate system, illustrated below at
     center-(0, 0) ~ corner-(0,1):

     |  (0,0)     (0,1)
     |      F-----7
     |      |    /|
     |    >-|--7  |
     |      |  |  |
     |      L-----J
     |  (1,0)  V  (1,1)

     If we were going anticlockwise, it'd be the opposite corner: (1, 0) *)
  let transform_clock_rel ~dir_in ~dir_out =
    match dir_in, dir_out with
    | Direction.(Up, Right) -> 0, 0
    | Up, Left -> 1, 0
    | Down, Right -> 0, 1
    | Down, Left -> 1, 1
    | Left, Up -> 1, 0
    | Left, Down -> 1, 1
    | Right, Down -> 0, 1
    | Right, Up -> 0, 0
    | _ -> failwith "not corner!"
  ;;

  let transform_counter_rel ~dir_in ~dir_out =
    let i, j = transform_clock_rel ~dir_in ~dir_out in
    1 - i, 1 - j
  ;;

  let transform ~wiseness ({ dir_in; dir_out; coords = i, j } as t) =
    let transform_rel =
      match wiseness with
      | `Clock -> transform_clock_rel
      | `Counter -> transform_counter_rel
    in
    let di, dj = transform_rel ~dir_in ~dir_out in
    { t with coords = i + di, j + dj }
  ;;
end

let construct_perim rows =
  let (_, final_dir), perim =
    List.fold_map
      rows
      ~init:((0, 0), Direction.Up)
      ~f:(fun (cur_coords, prev_dir) (cur_dir, count) ->
        let corner =
          Corner.{ coords = cur_coords; dir_in = prev_dir; dir_out = cur_dir }
        in
        let next_coords = Coords.move_n cur_coords cur_dir count in
        (next_coords, cur_dir), corner)
  in
  match perim with
  | [] -> []
  | hd :: tl ->
    let hd = Corner.{ hd with dir_in = final_dir } in
    hd :: tl
;;

let orientation perim =
  let least_window =
    Iteration.windows perim 3
    |> Sequence.max_elt ~compare:(fun a b ->
      let a = a.(1).Corner.coords in
      let b = b.(1).Corner.coords in
      Coords.compare a b)
    |> Option.value_exn
  in
  let a, b, c =
    least_window.(1).coords, least_window.(0).coords, least_window.(2).coords
  in
  let ab = Coords.(b - a |> to_floats) in
  let ac = Coords.(c - a |> to_floats) in
  if Coords.(cross_floats ab ac |> Float.compare 0. > 0) then `Clock else `Counter
;;

let area perim =
  let wiseness = orientation perim in
  let perim = List.map perim ~f:(Corner.transform ~wiseness) in
  let double_area =
    Iteration.windows perim 2
    |> Sequence.fold ~init:0 ~f:(fun acc window ->
      let (y, x), (y_next, x_next) = window.(0).Corner.coords, window.(1).Corner.coords in
      acc + ((x - x_next) * (y + y_next)))
  in
  double_area / 2
;;

let solve ~parse_row filename =
  In_channel.read_lines filename
  |> List.map ~f:parse_row
  |> construct_perim
  |> area
  |> sprintf !"%{sexp:(int)}"
;;

let part1 = solve ~parse_row:Parse.part1
let part2 = solve ~parse_row:Parse.part2
