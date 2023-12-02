open Advent2023.Day02

let filename = Format.sprintf "input/%s.txt" "day02"

let () =
  let sol1 = part1 filename in
  print_string sol1;
  print_newline ();
  let sol2 = part2 filename in
  print_string sol2;
  print_newline ()
