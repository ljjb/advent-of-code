type range =
  { lo : int
  ; hi : int
  }

let read_parsed_lines ~f filename =
  let ic = open_in filename in
  let f ic =
    match input_line ic with
    | line -> Some (f line, ic)
    | exception End_of_file -> None
  in
  Seq.unfold f ic |> Seq.filter_map (fun x -> x)
;;

let map2 ~f a b =
  match a, b with
  | Some a', Some b' -> Some (f a' b')
  | _ -> None
;;

let map2_id = map2 ~f:(fun x y -> x, y)
let map2_range = map2 ~f:(fun lo hi -> { lo; hi })

let read_range x =
  match String.split_on_char '-' x with
  | a :: b :: _ ->
    let lo = int_of_string_opt a
    and hi = int_of_string_opt b in
    map2_range lo hi
  | _ -> None
;;

let read_range_pair line =
  match String.split_on_char ',' line with
  | a :: b :: _ ->
    let a' = read_range a
    and b' = read_range b in
    map2_id a' b'
  | _ -> None
;;

let point_in_range p r = p >= r.lo && p <= r.hi
let range_in_range a b = point_in_range a.lo b && point_in_range a.hi b
let overlap_fully a b = range_in_range a b || range_in_range b a
let overlap a b = point_in_range a.lo b || point_in_range a.hi b || range_in_range b a

let count_overlaps ~f filename =
  let range_pair_seq = read_parsed_lines ~f:read_range_pair filename in
  let f acc (x, y) = acc + if f x y then 1 else 0 in
  Seq.fold_left f 0 range_pair_seq
;;

let part1 = count_overlaps ~f:overlap_fully
let part2 = count_overlaps ~f:overlap
