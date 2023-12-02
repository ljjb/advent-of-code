open Advent2023.Day01

let filename = Format.sprintf "input/%s.txt" "day01"

let () =
  let sol1 = part1 filename in
  print_int sol1;
  print_newline ();
  let sol1 = part2 filename in
  print_int sol1;
  print_newline ()
