open Core
open Components
open Angstrom
open Util.Parse

let ( &&> ), ( ||> ) = Util.(( &&> ), ( ||> ))

let condition =
  satisfy (fun c -> Char.(equal c '<' || equal c '>')) >>| Rule.condition_of_char
;;

let attribute_name =
  satisfy (fun c -> Char.(List.mem [ 'x'; 'm'; 'a'; 's' ] c ~equal)) >>| Rule.attr_of_char
;;

let attribute =
  let* name = attribute_name in
  let* _ = char '=' in
  let* value = integer in
  return (name, value)
;;

let machine_part =
  char '{' *> sep_by1 (char ',') attribute
  <* char '}'
  <* many any_char
  >>| Machine_part.of_alist
;;

let workflow_name = many1 (satisfy Char.(is_alpha &&> is_lowercase)) >>| String.of_list

let destination =
  let workflow_destination = workflow_name >>| fun name -> `Workflow name in
  let verdict_destination =
    satisfy (fun c -> Char.(equal c 'A' || equal c 'R'))
    >>| fun c -> `Verdict (if Char.equal c 'A' then true else false)
  in
  choice [ workflow_destination; verdict_destination ] ~failure_msg:"broken choice!"
;;

let rule =
  let* attribute = attribute_name in
  let* condition = condition in
  let* bound = integer in
  let* _ = char ':' in
  let* destination = destination in
  return Rule.{ attribute; condition; bound; destination }
;;

let rules = sep_by1 (char ',') rule

let workflow =
  let workflow_contents =
    rules
    <* char ','
    >>= fun rules -> destination >>= fun default -> return Workflow.{ rules; default }
  in
  char '{' *> workflow_contents <* char '}'
;;

let workflow_with_name =
  let* name = spaces *> workflow_name in
  let* workflow = workflow <* many any_char in
  return (name, workflow)
;;

let parse_destination = parse_general destination
let parse_rule = parse_general rule
let parse_rules = parse_general rules
let parse_workflow_with_name = parse_general workflow_with_name
let parse_machine_part = parse_general machine_part

let%expect_test "workflow_with_name" =
  "px{a<2006:qkq,m>2090:A,rfg}"
  |> parse_workflow_with_name
  |> printf !"%{sexp:(string * Workflow.t)}";
  [%expect
    {|
    (px
     ((rules
       (((attribute Aero) (condition Less) (bound 2006)
         (destination (Workflow qkq)))
        ((attribute Musical) (condition Greater) (bound 2090)
         (destination (Verdict true)))))
      (default (Workflow rfg)))) |}]
;;

let%expect_test "destination" =
  "A"
  |> parse_destination
  |> printf !"%{sexp:([ `Verdict of bool | `Workflow of string ])}";
  [%expect "(Verdict true)"]
;;

let%expect_test "rules" =
  "a<2006:qkq,m>2090:A" |> parse_rules |> printf !"%{sexp:(Rule.t list)}";
  [%expect
    {|
    (((attribute Aero) (condition Less) (bound 2006)
      (destination (Workflow qkq)))
     ((attribute Musical) (condition Greater) (bound 2090)
      (destination (Verdict true)))) |}]
;;

let%expect_test "attributes" =
  "{x=787,m=2655,a=1222,s=2876}"
  |> parse_machine_part
  |> printf !"%{sexp:(Machine_part.t)}";
  [%expect "((xtreme 787) (musical 2655) (aero 1222) (shiny 2876))"]
;;
