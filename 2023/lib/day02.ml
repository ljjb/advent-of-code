open Core
open Stdio

module Outcome = struct
  type t = { red : int; green : int; blue : int } [@@deriving sexp]

  let empty = { red = 0; green = 0; blue = 0 }

  let add_polyvar t x =
    match x with
    | `Red i -> { t with red = i }
    | `Green i -> { t with green = i }
    | `Blue i -> { t with blue = i }

  let of_list = List.fold ~init:empty ~f:add_polyvar

  let ( - ) x y =
    let red = x.red - y.red in
    let green = x.green - y.green in
    let blue = x.blue - y.blue in
    { red; green; blue }

  let any_negative t = t.red < 0 || t.blue < 0 || t.green < 0

  let piecewise_max x y =
    let red = Int.max x.red y.red in
    let green = Int.max x.green y.green in
    let blue = Int.max x.blue y.blue in
    { red; green; blue }

  let power t = t.red * t.green * t.blue
end

module Parse = struct
  open Angstrom

  let game_number = string "Game " *> take_while Char.is_digit *> string ": "

  let number =
    let* digits = take_while Char.is_digit in
    return @@ Int.of_string digits

  let many_space = many @@ char ' '

  let red =
    many_space *> number <* string " red" <* many_space >>| fun i -> `Red i

  let green =
    many_space *> number <* string " green" <* many_space >>| fun i -> `Green i

  let blue =
    many_space *> number <* string " blue" <* many_space >>| fun i -> `Blue i

  let color = choice [ red; green; blue ]
  let outcome = sep_by (char ',') color >>| fun xs -> Outcome.of_list xs
  let outcomes = sep_by (char ';') outcome
  let game = game_number *> outcomes

  let game_of_string line =
    match parse_string ~consume:Consume.All game line with
    | Ok game -> game
    | _ -> failwith "Invalid input"
end

let possible_given_outcomes total outcomes =
  let pred outcome =
    let open Outcome in
    let diff = total - outcome in
    any_negative diff
  in
  not @@ List.exists outcomes ~f:pred

let power outcomes =
  let minimum_cubes =
    List.fold outcomes ~init:Outcome.empty ~f:Outcome.piecewise_max
  in
  Outcome.power minimum_cubes

let solve part filename =
  let filter_map =
    match part with
    | `One ->
        let bag = Outcome.{ red = 12; green = 13; blue = 14 } in
        Sequence.filter_mapi ~f:(fun i x ->
            match possible_given_outcomes bag x with
            | true -> Some (i + 1)
            | false -> None)
    | `Two -> Sequence.map ~f:power
  in
  let f ic =
    let puzzle =
      Util.read_lines ic
      |> Sequence.map ~f:Parse.game_of_string
      |> filter_map
      |> Sequence.sum (module Int) ~f:Util.id
    in
    sprintf !"%{sexp:(Int.t)}" puzzle
  in
  In_channel.with_file filename ~f

let part1 = solve `One
let part2 = solve `Two
