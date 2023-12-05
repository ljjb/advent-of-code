open Core

module Range = struct
  type t =
    { src_pos : int
    ; dst_pos : int
    ; len : int
    }
  [@@deriving sexp]

  let find t ~src =
    if src < t.src_pos
    then None
    else if src < t.src_pos + t.len
    then Some (src - (t.src_pos - t.dst_pos))
    else None
  ;;
end

module Range_map = struct
  type t =
    { from_name : string
    ; to_name : string
    ; ranges : Range.t List.t
    }
  [@@deriving sexp]

  let find t src =
    List.find_map t.ranges ~f:(Range.find ~src) |> Option.value ~default:src
  ;;

  let map_of_list ts =
    let map = Map.empty (module String) in
    List.fold ts ~init:map ~f:(fun acc t ->
      match Map.add acc ~key:t.from_name ~data:t with
      | `Ok acc' -> acc'
      | `Duplicate -> acc)
  ;;
end

module Parse = struct
  open Angstrom

  let ms = many @@ char ' '
  let newline = char '\n'
  let newline_sep = newline *> newline
  let number = take_while1 Char.is_digit >>| Int.of_string
  let seeds = string "seeds:" *> ms *> sep_by1 ms number <* ms

  let names =
    let* from_name = take_while1 Char.is_alpha in
    let* _ = string "-to-" in
    let* to_name = take_while1 Char.is_alpha in
    let* _ = ms *> string "map:" *> ms in
    return (from_name, to_name)
  ;;

  let range =
    let* dst_pos = ms *> number in
    let* src_pos = ms *> number in
    let* len = ms *> number <* ms in
    return Range.{ src_pos; dst_pos; len }
  ;;

  let ranges = sep_by1 newline range

  let range_map =
    let* from_name, to_name = names in
    let* _ = newline in
    let* ranges = ranges in
    return Range_map.{ from_name; to_name; ranges }
  ;;

  let range_maps = sep_by1 newline_sep range_map <* take_while (fun _ -> true)

  let puzzle =
    let* seeds = seeds in
    let* _ = newline_sep in
    let* range_maps = range_maps in
    return (seeds, range_maps)
  ;;
end

let parse_general parser input =
  let open Angstrom in
  (match parse_string ~consume:Consume.All parser input with
   | Ok parsed -> Some parsed
   | _ -> None)
  |> Option.value_exn
;;

let parse_puzzle = parse_general Parse.puzzle
let parse_seeds = parse_general Parse.seeds
let parse_names = parse_general Parse.names
let parse_range = parse_general Parse.range
let parse_range_map = parse_general Parse.range_map

let%expect_test "seeds" =
  "seeds: 79 14 55 13" |> parse_seeds |> printf !"%{sexp:(Int.t List.t)}";
  [%expect {| (79 14 55 13) |}]
;;

let%expect_test "names" =
  "light-to-temperature map:" |> parse_names |> printf !"%{sexp:(string * string)}";
  [%expect {| (light temperature)|}]
;;

let%expect_test "range" =
  "45 77 23" |> parse_range |> printf !"%{sexp:(Range.t)}";
  [%expect "((src_pos 77) (dst_pos 45) (len 23))"]
;;

let%expect_test "puzzle" =
  "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n"
  |> parse_puzzle
  |> printf !"%{sexp:((Int.t List.t) * (Range_map.t List.t))}";
  [%expect
    {|
    ((79 14 55 13)
     (((from_name seed) (to_name soil)
       (ranges
        (((src_pos 98) (dst_pos 50) (len 2))
         ((src_pos 50) (dst_pos 52) (len 48)))))))
   |}]
;;

let%expect_test "range_map" =
  "light-to-temperature map:\n45 77 23\n81 45 19\n68 64 13"
  |> parse_range_map
  |> printf !"%{sexp:(Range_map.t)}";
  [%expect
    {|
    ((from_name light) (to_name temperature)
     (ranges
      (((src_pos 77) (dst_pos 45) (len 23)) ((src_pos 45) (dst_pos 81) (len 19))
       ((src_pos 64) (dst_pos 68) (len 13)))))
   |}]
;;

let map_keys =
  [ "seed"; "soil"; "fertilizer"; "water"; "light"; "temperature"; "humidity" ]
;;

let make_range lo len =
  let hix = lo + len in
  Sequence.unfold ~init:lo ~f:(fun lo -> if lo < hix then Some (lo, lo + 1) else None)
;;

let%expect_test "make_range" =
  make_range 79 14 |> Sequence.to_list |> printf !"%{sexp:(Int.t List.t)}";
  [%expect "((src_pos 77) (dst_pos 45) (len 23))"]
;;

let make_ranges seeds =
  let chunks_of_size = seeds |> Sequence.of_list |> Sequence.chunks_exn in
  chunks_of_size 2
  |> Sequence.map ~f:(function
    | [ lo; len ] -> make_range lo len
    | _ -> failwith "Bzzzt! Wrongo!")
  |> Sequence.concat
;;

let solve part filename =
  let contents = In_channel.(with_file filename ~f:input_all) in
  let seeds, range_maps = contents |> parse_puzzle in
  (* Part two took 25 mins to run lol *)
  let seeds =
    match part with
    | `One -> Sequence.of_list seeds
    | `Two -> make_ranges seeds
  in
  let range_map_map = Range_map.map_of_list range_maps in
  let destinations =
    Sequence.map seeds ~f:(fun seed ->
      List.fold map_keys ~init:seed ~f:(fun acc cur ->
        let cur_map = Map.find_exn range_map_map cur in
        Range_map.find cur_map acc))
  in
  destinations
  |> Sequence.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> sprintf !"%{sexp:(int)}"
;;

let part1 = solve `One
let part2 = solve `Two
