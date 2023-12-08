open Advent2023.Day08

let () =
  let filename = "input/day08.txt" in
  let sol1 = part1 filename in
  print_string sol1;
  Out_channel.(flush stdout);
  let sol2 = part2 filename in
  Format.printf "%s\n" sol2
;;
