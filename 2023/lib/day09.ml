open Core
open Stdio

module Parse = struct
  open Angstrom
  open Util.Parse

  let numbers = integers
  let puzzle = many lenient_endl *> sep_by1 lenient_endl numbers <* many any_char
  let parse = parse_general puzzle
end

let part1 filename =
  let puzzle = In_channel.read_all filename |> Parse.parse in
  puzzle |> sprintf !"%{sexp:(int list list)}\n"
;;
