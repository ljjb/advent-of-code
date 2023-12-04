open Core
open Stdio

module Card = struct
  type t =
    { idx : int
    ; winners : int list
    ; received : int list
    }
  [@@deriving sexp]

  let num_matching t =
    List.filter t.received ~f:(fun x -> List.mem t.winners x ~equal:Int.equal)
    |> List.length
  ;;

  let won_shallow t =
    let n = num_matching t in
    List.init n ~f:(fun x -> t.idx + x + 1)
  ;;
end

module Parse = struct
  open Angstrom

  let many_space = many @@ char ' '
  let integer = take_while1 Char.is_digit >>| Int.of_string
  let numbers = many_space *> sep_by many_space integer <* many_space
  let card_label = string_ci "Card" *> many_space *> integer <* char ':'

  let left_and_right =
    let* left = numbers in
    let* _ = char '|' in
    let* right = numbers in
    return (left, right)
  ;;

  let card =
    let* idx = card_label in
    let* winners, received = left_and_right in
    return Card.{ idx; winners; received }
  ;;

  let card_of_string line =
    match parse_string ~consume:Consume.All card line with
    | Ok game -> game
    | _ -> failwith "Invalid input"
  ;;
end

let points_of_num_matching x = if x <= 0 then 0 else Int.pow 2 (x - 1)

let part1 filename =
  let f ic =
    Util.read_lines ic
    |> Sequence.map ~f:Parse.card_of_string
    |> Sequence.map ~f:Card.num_matching
    |> Sequence.map ~f:points_of_num_matching
    |> Sequence.sum (module Int) ~f:Util.id
    |> sprintf !"%{sexp:(Int.t)}"
  in
  In_channel.with_file filename ~f
;;

let part2 filename =
  let f ic =
    let card_wins = Hashtbl.create (module Int) in
    let deep_card_wins = Hashtbl.create (module Int) in
    let cards =
      Util.read_lines ic |> Sequence.to_list |> List.map ~f:Parse.card_of_string
    in
    cards
    |> List.iter ~f:(fun card ->
      Hashtbl.update card_wins card.idx ~f:(function
        | None -> Card.won_shallow card
        | Some matching -> matching));
    let rec won_by idx =
      match Hashtbl.find_exn card_wins idx with
      | [] -> 0
      | many ->
        let n = List.length many in
        List.fold many ~init:n ~f:(fun acc x ->
          (* printf !"updating %{sexp:(int)}\n" x; *)
          Hashtbl.update deep_card_wins x ~f:(function
            | None -> won_by x
            | Some num -> num);
          acc + Hashtbl.find_exn deep_card_wins x)
    in
    List.fold cards ~init:(List.length cards) ~f:(fun acc card -> acc + won_by card.idx)
    |> sprintf !"%{sexp:(Int.t)}"
  in
  In_channel.with_file filename ~f
;;