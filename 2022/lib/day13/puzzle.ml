open Base
open Stdio

type t =
  | Many of t list
  | Single of int

let rec equal a b =
  match a, b with
  | Single a', Single b' -> Int.(a' = b')
  | Many a', Many b' -> List.equal equal a' b'
  | _ -> false
;;

module Parse = struct
  let puzzle_int_of_string x = Single (Int.of_string x)
  let puzzle_list_of_string x = Many x

  let integer =
    Angstrom.(
      take_while1 (function
        | '0' .. '9' -> true
        | _ -> false)
      >>| puzzle_int_of_string)
  ;;

  let many_spaces = Angstrom.(many (char ' '))
  let spaced_char c = Angstrom.(many_spaces *> char c <* many_spaces)
  let square_brackets b = Angstrom.(char '[' *> b <* char ']')

  let puzzle_list =
    let open Angstrom in
    fix (fun weird_list ->
      spaced_char '[' *> sep_by (spaced_char ',') (weird_list <|> integer)
      <* spaced_char ']'
      >>| puzzle_list_of_string)
  ;;
end

let parse_puzzle_list input =
  let open Angstrom in
  parse_string ~consume:All Parse.puzzle_list input
;;

let pair_seq_of_ic init =
  let f ic =
    let module Step = Sequence.Step in
    match In_channel.input_line ic with
    | None -> Step.Done
    | Some "" -> Step.Skip { state = ic }
    | Some line_one ->
      (match In_channel.input_line ic with
       | None -> Step.Done
       | Some "" -> Step.Skip { state = ic }
       | Some line_two ->
         (match parse_puzzle_list line_one, parse_puzzle_list line_two with
          | Ok line_one, Ok line_two ->
            Step.Yield { value = line_one, line_two; state = ic }
          | _ -> Step.Skip { state = ic }))
  in
  Sequence.unfold_step ~init ~f
;;

let%test "simple list" =
  match parse_puzzle_list "[1,1,5,1,1]" with
  | Ok plist -> equal plist (Many [ Single 1; Single 1; Single 5; Single 1; Single 1 ])
  | _ -> false
;;

let%test "nested list" =
  match parse_puzzle_list "[1,[8],5,[[]],1,1]" with
  | Ok plist ->
    equal
      plist
      (Many
         [ Single 1; Many [ Single 8 ]; Single 5; Many [ Many [] ]; Single 1; Single 1 ])
  | _ -> false
;;
