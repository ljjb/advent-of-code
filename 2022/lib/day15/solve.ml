open Core

let ( %> ) g f x = x |> g |> f
let manhattan (ax, ay) (bx, by) = Int.(abs (ax - bx) + abs (ay - by))

module Diamond = struct
  type t =
    { sensor : int * int
    ; beacon : int * int
    ; range : int
    }
  [@@deriving sexp]

  let of_coords (sensor, beacon) =
    let range = manhattan sensor beacon in
    let out = { sensor; beacon; range } in
    out
  ;;

  let range_at_row t ~row_y =
    let { range; sensor = cx, cy; _ } = t in
    let dy = Int.abs (row_y - cy) in
    let offset = range - dy in
    if offset < 0 then None else Some (cx - offset, cx + offset)
  ;;

  let inside t coords = manhattan t.sensor coords <= t.range
end

let point_in (lo, hi) x = x >= lo && x <= hi
let range_in ~outer:(olo, ohi) ~inner:(ilo, ihi) = ilo >= olo && ihi <= ohi

let merge ((alo, ahi) as a) ((blo, bhi) as b) =
  if point_in b alo || point_in b ahi
  then
    if range_in ~outer:b ~inner:a then Some b else Some (Int.min alo blo, Int.max ahi bhi)
  else if range_in ~outer:a ~inner:b
  then Some a
  else None
;;

let add_interval bs a =
  let rec aux bs a acc =
    match bs with
    | [] -> a :: acc
    | hd :: tl ->
      (match merge a hd with
       | None -> aux tl a (hd :: acc)
       | Some a' -> aux tl a' acc)
  in
  if List.length bs = 0 then [ a ] else aux bs a [] |> List.rev
;;

let part1 filename =
  let row_y = 2000000 in
  let num_covered (lo, hi) = Int.abs (lo - hi) + 1 in
  let diamonds = Parse.parse filename |> List.map ~f:Diamond.of_coords in
  let beacons_in_row =
    diamonds
    |> List.filter_map ~f:(fun ele ->
      if snd ele.beacon = row_y then Some ele.beacon else None)
    |> List.dedup_and_sort ~compare:(Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare)
    |> List.length
  in
  diamonds
  |> List.filter_map ~f:(Diamond.range_at_row ~row_y)
  |> List.fold ~init:[] ~f:(fun acc range -> add_interval acc range)
  |> List.fold ~init:0 ~f:(fun acc ele -> acc + num_covered ele)
  |> fun x -> x - beacons_in_row
;;

(* Part 2: Search in intersections of perimeters of diamonds. *)

module Flank = struct
  (* Represents a line segment *)
  type t =
    { x : int
    ; y : int
    ; c : int (* as in y = mx + c *)
    ; span : int
    }
  [@@deriving sexp, compare]

  let of_points (ax, ay) (bx, by) =
    let (ax, ay), (_, by) = if bx < ax then (bx, by), (ax, ay) else (ax, ay), (bx, by) in
    (* let span_x = bx - ax in *)
    let span_y = by - ay in
    (* let m = span_y / span_x in *)
    (* assert (not (ax - bx = 0)); *)
    (* assert (m = -1 || m = 1); *)
    if by > ay
    then { x = ax; y = ay; c = ay - ax; span = span_y }
    else { x = ax; y = ay; c = ay + ax; span = span_y }
  ;;

  let intersection a b =
    let c1_p_c2 = a.c + b.c in
    if not @@ (c1_p_c2 mod 2 = 0)
    then None
    else (
      let intr_y = c1_p_c2 / 2 in
      let intr_x = if a.span < 0 then intr_y - b.c else b.c - intr_y in
      (* No need to check bounding box because there's only one solution
         to the puzzle anyway and manhattan distance is very cheap *)
      Some (intr_x, intr_y))
  ;;
end

module Perimeter = struct
  type t =
    { nw : Flank.t
    ; se : Flank.t
    ; sw : Flank.t
    ; ne : Flank.t
    ; diamond : Diamond.t
    }
  [@@deriving sexp]

  let of_diamond diamond =
    let open Diamond in
    let dist = diamond.range + 1 in
    let cx, cy = diamond.sensor in
    let nx, ny = cx, cy + dist in
    let ex, ey = cx + dist, cy in
    let sx, sy = cx, cy - dist in
    let wx, wy = cx - dist, cy in
    let nw = Flank.of_points (wx, wy) (nx, ny) in
    let se = Flank.of_points (sx, sy) (ex, ey) in
    let sw = Flank.of_points (wx, wy) (sx, sy) in
    let ne = Flank.of_points (nx, ny) (ex, ey) in
    { nw; se; sw; ne; diamond }
  ;;
end

let part2 filename =
  let lo_bound = 0 in
  let hi_bound = 4000000 in
  let flanks_list =
    Parse.parse filename |> List.map ~f:(Diamond.of_coords %> Perimeter.of_diamond)
  in
  let outside_all_diamonds coords =
    not
    @@ List.exists flanks_list ~f:(fun some_flanks ->
      Diamond.inside some_flanks.diamond coords)
  in
  (* Find intersections of one diamond with all others *)
  let find_intersection others_list flanks =
    let open Perimeter in
    List.find_map others_list ~f:(fun others ->
      List.find_map
        [ flanks.nw, others.sw
        ; flanks.nw, others.ne
        ; flanks.se, others.sw
        ; flanks.se, others.ne
        ]
        ~f:(fun (a, b) ->
          Flank.intersection a b
          |> Option.find_map ~f:(fun ((cx, cy) as coords) ->
            if outside_all_diamonds coords
               && point_in (lo_bound, hi_bound) cx
               && point_in (lo_bound, hi_bound) cy
            then Some coords
            else None)))
  in
  (* O(N^2) part *)
  let coords_opt =
    List.find_mapi flanks_list ~f:(fun i flanks ->
      let others_list = List.filteri flanks_list ~f:(fun j _ -> not (i = j)) in
      find_intersection others_list flanks)
  in
  let coords_opt =
    match coords_opt with
    | Some _ as some_coords -> some_coords
    | None ->
      (* Search the corners of the space, because those are the only
         places where a non-intersection could be the only unseen tile *)
      List.find
        [ lo_bound, lo_bound; lo_bound, hi_bound; hi_bound, hi_bound; hi_bound, lo_bound ]
        ~f:outside_all_diamonds
  in
  match coords_opt with
  | None -> "None"
  | Some (x, y) -> sprintf "%i" @@ ((x * hi_bound) + y)
;;
