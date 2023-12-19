open Core
open Angstrom
open Util.Parse

let direction =
  satisfy Char.(List.mem [ 'U'; 'D'; 'L'; 'R' ] ~equal) >>| Direction.of_char
;;

let hex_digit = satisfy Char.is_hex_digit >>| Char.get_hex_digit_exn

let hex_digts =
  let* digits = list @@ List.init 5 ~f:(fun _ -> hex_digit) in
  let count =
    List.rev digits
    |> List.foldi ~init:0 ~f:(fun i acc digit -> (Int.pow 16 i * digit) + acc)
  in
  return count
;;

let primary =
  let* most_sig = hex_digit in
  let* least_sig = hex_digit in
  return @@ ((most_sig lsl 4) + least_sig)
;;

let direction_code =
  satisfy Char.(List.mem [ '0'; '1'; '2'; '3' ] ~equal) >>| Direction.of_char
;;

let colour_code =
  string "(#" *> both hex_digts direction_code
  <* char ')'
  >>| fun (count, direction) -> direction, count
;;

let row_part1 =
  let* direction = spaces *> direction in
  let* _ = spaces1 in
  let* count = integer in
  let* _ = spaces1 in
  let* _ = colour_code <* spaces in
  return (direction, count)
;;

let row_part2 =
  spaces *> direction *> spaces1 *> integer *> spaces1 *> colour_code <* spaces
;;

let part1 = parse_general row_part1
let part2 = parse_general row_part2
