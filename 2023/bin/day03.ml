open Advent2023.Day03

let filename = Format.sprintf "input/%s.txt" "day03"

let () =
  let sol1 = part1 filename in
  print_string sol1;
  print_newline ()
;;

let sol2 = part2 filename in
print_int sol2;
print_newline ()
