open Advent2023.Day06

let () =
  let filename = "input/day06.txt" in
  let sol1 = part1 filename in
  print_string sol1;
  Out_channel.(flush stdout);
  let sol2 = part2 filename in
  Format.printf "%s\n" sol2
;;
