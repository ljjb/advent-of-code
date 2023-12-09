open Core
open Stdio

module Outcome = struct
  type t =
    { red : int
    ; green : int
    ; blue : int
    }
  [@@deriving sexp]

  let empty = { red = 0; green = 0; blue = 0 }

  let add_polyvar t x =
    match x with
    | `Red i -> { t with red = i }
    | `Green i -> { t with green = i }
    | `Blue i -> { t with blue = i }
  ;;

  let of_list = List.fold ~init:empty ~f:add_polyvar

  let ( - ) x y =
    let red = x.red - y.red in
    let green = x.green - y.green in
    let blue = x.blue - y.blue in
    { red; green; blue }
  ;;

  let any_negative t = t.red < 0 || t.blue < 0 || t.green < 0

  let piecewise_max x y =
    let red = Int.max x.red y.red in
    let green = Int.max x.green y.green in
    let blue = Int.max x.blue y.blue in
    { red; green; blue }
  ;;

  let power t = t.red * t.green * t.blue
end

module Parse = struct
  open Angstrom
  open Util.Parse

  let game_number = string "Game " *> integer *> string ": "
  let red = integer <* string " red" >>| fun i -> `Red i
  let green = integer <* string " green" >>| fun i -> `Green i
  let blue = integer <* string " blue" >>| fun i -> `Blue i
  let color = spaces *> choice [ red; green; blue ] <* spaces
  let outcome = sep_by (char ',') color >>| fun xs -> Outcome.of_list xs
  let outcomes = sep_by (char ';') outcome
  let puzzle = game_number *> outcomes
  let parse = parse_general puzzle
end

let possible_given bag outcomes =
  let pred outcome = Outcome.(any_negative (bag - outcome)) in
  not @@ List.exists outcomes ~f:pred
;;

let power outcomes = Outcome.(List.fold outcomes ~init:empty ~f:piecewise_max |> power)

let solve part filename =
  let filter_map =
    match part with
    | `One ->
      let bag = Outcome.{ red = 12; green = 13; blue = 14 } in
      Sequence.filter_mapi ~f:(fun i x ->
        match possible_given bag x with
        | true -> Some (i + 1)
        | false -> None)
    | `Two -> Sequence.map ~f:power
  in
  let f ic =
    let puzzle =
      Util.read_lines ic
      |> Sequence.map ~f:Parse.parse
      |> filter_map
      |> Sequence.sum (module Int) ~f:Util.id
    in
    sprintf !"%{sexp:(Int.t)}" puzzle
  in
  In_channel.with_file filename ~f
;;

let part1 = solve `One
let part2 = solve `Two
