open Advent2023.Day11

let () =
  let filename = "input/day11.txt" in
  let sol1 = part1 filename in
  Format.printf "%s\n" sol1;
  Out_channel.(flush stdout);
  let sol2 = part2 filename in
  Format.printf "%s\n" sol2
;;
