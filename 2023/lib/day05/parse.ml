open Core
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
  return @@ Range.make ~src_pos ~dst_pos ~len
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

let parse_general parser input =
  let open Angstrom in
  (match parse_string ~consume:Consume.All parser input with
   | Ok parsed -> Some parsed
   | _ -> None)
  |> Option.value_exn
;;

let parse_puzzle = parse_general puzzle
let parse_seeds = parse_general seeds
let parse_names = parse_general names
let parse_range = parse_general range
let parse_range_map = parse_general range_map

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
  [%expect "((src_pos 77) (dst_pos 45) (len 23) (delta -32))"]
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
        (((src_pos 98) (dst_pos 50) (len 2) (delta -48))
         ((src_pos 50) (dst_pos 52) (len 48) (delta 2)))))))
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
      (((src_pos 77) (dst_pos 45) (len 23) (delta -32))
       ((src_pos 45) (dst_pos 81) (len 19) (delta 36))
       ((src_pos 64) (dst_pos 68) (len 13) (delta 4)))))
   |}]
;;
