open Core
open Stdio

let read_puzzle ic =
  let f ic =
    match In_channel.input_line ic with
    | Some line -> Some (line, ic)
    | None -> None
  in
  Sequence.unfold ~init:ic ~f

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
  module A = Angstrom

  let game_number =
    A.(string "Game " *> A.take_while Char.is_digit *> string ": ")

  let number =
    let open A in
    let* digits = take_while Char.is_digit in
    return @@ Int.of_string digits

  let many_space = A.(many @@ char ' ')

  let red =
    let open A in
    many_space *> number <* string " red" <* many_space >>| fun i -> `Red i

  let green =
    let open A in
    many_space *> number <* string " green" <* many_space >>| fun i -> `Green i

  let blue =
    let open A in
    many_space *> number <* string " blue" <* many_space >>| fun i -> `Blue i

  let color = A.(choice [ red; green; blue ])

  let outcome =
    let open A in
    sep_by (char ',') color >>| fun xs -> Outcome.of_list xs

  let outcomes =
    let open A in
    sep_by (char ';') outcome

  let game = A.(game_number *> outcomes)
end

let game_of_string line =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All Parse.game line with
  | Ok game -> game
  | _ -> failwith "Invalid input"

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

let id x = x

let part1 filename =
  let gold = Outcome.{ red = 12; green = 13; blue = 14 } in
  In_channel.with_file filename ~f:(fun ic ->
      let puzzle =
        read_puzzle ic
        |> Sequence.map ~f:game_of_string
        |> Sequence.filter_mapi ~f:(fun i x ->
               match possible_given_outcomes gold x with
               | true -> Some (i + 1)
               | false -> None)
        |> Sequence.sum (module Int) ~f:id
      in
      sprintf !"%{sexp:(Int.t)}" puzzle)

let part2 filename =
  In_channel.with_file filename ~f:(fun ic ->
      let puzzle =
        read_puzzle ic
        |> Sequence.map ~f:game_of_string
        |> Sequence.map ~f:power
        |> Sequence.sum (module Int) ~f:id
      in
      sprintf !"%{sexp:(Int.t)}" puzzle)
