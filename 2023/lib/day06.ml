open Core
open Stdio

module Parse = struct
  open Angstrom
  open Util.Parse

  let lenient_endl = spaces *> endl <* spaces

  module Part1 = struct
    let times = string_ci "time:" *> spaces1 *> integers
    let distances = string_ci "distance:" *> spaces1 *> integers

    let puzzle =
      let* times = many lenient_endl *> times in
      let* _ = many1 lenient_endl in
      let* distances = distances <* many any_char in
      return @@ List.zip_exn times distances
    ;;

    let parse = parse_general puzzle
  end

  module Part2 = struct
    let kerned_integer =
      sep_by1 (skip_while (Char.equal ' ')) (take_while1 Char.is_digit)
      >>| fun xs -> String.concat xs |> Int.of_string
    ;;

    let time = string_ci "time:" *> spaces *> kerned_integer
    let distance = string_ci "distance:" *> spaces *> kerned_integer

    let puzzle =
      let* time = many lenient_endl *> time in
      let* _ = many1 lenient_endl in
      let* distance = distance <* many any_char in
      return (time, distance)
    ;;

    let parse = parse_general puzzle
  end

  let parse_part1 = Part1.parse
  let parse_part2 = Part2.parse
end

let ways_to_win limit_ms hurdle_dist =
  let dist accel_ms = (limit_ms - accel_ms) * accel_ms in
  let rec aux acc accel_ms =
    if accel_ms > limit_ms
    then acc
    else (
      let cur_dist = dist accel_ms in
      let acc = if cur_dist > hurdle_dist then acc + 1 else acc in
      aux acc (accel_ms + 1))
  in
  aux 0 0
;;

(* Solution using quadratic equation *)
let ways_to_win' limit_ms hurdle_dist =
  let limit_ms, hurdle_dist = Float.(of_int limit_ms, of_int hurdle_dist) in
  let opti = limit_ms /. 2.0 in
  let radius = Float.sqrt ((limit_ms *. limit_ms) -. (4.0 *. hurdle_dist)) /. 2.0 in
  let float_lo, float_hi =
    Float.(round_up (opti -. radius), round_down (opti +. radius))
  in
  let lo, hi = Int.(of_float float_lo, of_float float_hi) in
  if hi < lo then 0 else hi - lo + 1
;;

let part1 filename =
  let puzzle = In_channel.read_all filename |> Parse.parse_part1 in
  List.fold puzzle ~init:1 ~f:(fun acc (limit_ms, hurdle_dist) ->
    acc * ways_to_win limit_ms hurdle_dist)
  |> sprintf !"%{sexp:(int)}\n"
;;

let part2 filename =
  let limit_ms, hurdle_dist = In_channel.read_all filename |> Parse.parse_part2 in
  let answer = ways_to_win' limit_ms hurdle_dist in
  answer |> sprintf !"%{sexp:(int)}\n"
;;
