open Core
module Step = Sequence.Step

type choice =
  | Rock
  | Paper
  | Scissors
[@@deriving equal]

type outcome =
  | Loss
  | Draw
  | Win

let choice_of_string_opt = function
  | "A" | "X" -> Some Rock
  | "B" | "Y" -> Some Paper
  | "C" | "Z" -> Some Scissors
  | _ -> None
;;

let outcome_of_string_opt = function
  | "X" -> Some Loss
  | "Y" -> Some Draw
  | "Z" -> Some Win
  | _ -> None
;;

let choices_of_line_opt line =
  let chunks = String.split ~on:' ' line in
  match chunks with
  | h1 :: h2 :: _ ->
    Option.map2 (choice_of_string_opt h1) (choice_of_string_opt h2) ~f:(fun a b -> a, b)
  | _ -> None
;;

let instructions_of_line_opt line =
  let chunks = String.split ~on:' ' line in
  match chunks with
  | h1 :: h2 :: _ ->
    Option.map2 (choice_of_string_opt h1) (outcome_of_string_opt h2) ~f:(fun a b -> a, b)
  | _ -> None
;;

let read_rps f filename =
  let init = In_channel.create filename
  and f' in_channel =
    match In_channel.input_line in_channel with
    | Some line ->
      Option.value_map
        (f line)
        ~f:(fun parsed -> Step.Yield { state = in_channel; value = parsed })
        ~default:(Step.Skip { state = in_channel })
    | None ->
      In_channel.close in_channel;
      Step.Done
  in
  Sequence.unfold_step ~init ~f:f'
;;

let score_of_choice = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3
;;

let score_of_outcome = function
  | Loss -> 0
  | Draw -> 3
  | Win -> 6
;;

let outcome_of_choices (theirs, mine) =
  match theirs, mine with
  | a, b when equal_choice a b -> Draw
  | Paper, Rock | Scissors, Paper | Rock, Scissors -> Loss
  | _ -> Win
;;

let choice_of_theirs_and_outcome (theirs, outcome) =
  match theirs, outcome with
  | Rock, Loss | Paper, Win | Scissors, Draw -> Scissors
  | Paper, Loss | Scissors, Win | Rock, Draw -> Rock
  | _ -> Paper
;;

let score_of_choices (theirs, mine) =
  let bonus_score = score_of_choice mine
  and outcome_score = (theirs, mine) |> outcome_of_choices |> score_of_outcome in
  bonus_score + outcome_score
;;

let score_of_instructions (theirs, outcome) =
  let outcome_score = score_of_outcome outcome in
  let mine = choice_of_theirs_and_outcome (theirs, outcome) in
  let bonus_score = score_of_choice mine in
  bonus_score + outcome_score
;;

let score_sum ~parse_f ~score_f filename =
  let parsed_stream = read_rps parse_f filename
  and init = 0
  and f acc choices = acc + score_f choices in
  Sequence.fold parsed_stream ~init ~f
;;

let part1 = score_sum ~parse_f:choices_of_line_opt ~score_f:score_of_choices
let part2 = score_sum ~parse_f:instructions_of_line_opt ~score_f:score_of_instructions
