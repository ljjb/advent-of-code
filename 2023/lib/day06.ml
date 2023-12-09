open Core

module Parse = struct
  open Angstrom
  open Util.Parse

  let lenient_endl = spaces *> endl <* spaces
  let times = string_ci "time:" *> spaces1 *> integers
  let distances = string_ci "distance:" *> spaces1 *> integers

  let puzzle =
    let* times = many lenient_endl *> times in
    let* _ = many1 lenient_endl in
    let* distances = distances <* many any_char in
    return @@ List.zip_exn times distances
  ;;

  let parse = parse_general puzzle
  let parse_times = parse_general times
  let parse_distances = parse_general distances
end

let%expect_test "times" =
  let input = "Time:      7  15   30" in
  Parse.parse_times input |> printf !"%{sexp:(int list)}";
  [%expect "(7 15 30)"]
;;

let%expect_test "distances" =
  let input = "Distance:      9  40  200" in
  Parse.parse_distances input |> printf !"%{sexp:(int list)}";
  [%expect "(9 40 200)"]
;;

let%expect_test "puzzle" =
  let input = "Time:      7  15   30\nDistance:      9  40  200\n" in
  Parse.parse input |> printf !"%{sexp:((int * int) list)}";
  [%expect "((7 9) (15 40) (30 200))"]
;;

let part1 filename =
  let puzzle = In_channel.read_all filename |> Parse.parse in
  puzzle |> sprintf !"%{sexp:((int * int) list)}\n"
;;
