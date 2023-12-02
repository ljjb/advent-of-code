open Advent2022.Day02

let () =
  let filename = "input/day02.txt" in
  let sol1, sol2 = (part1 filename, part2 filename) in
  Format.printf "%d\n%d\n" sol1 sol2
