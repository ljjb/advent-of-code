open Core
open Stdio

module Card = struct
  type t =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Tercio
    | Jack
    | Queen
    | King
    | Ace
  [@@deriving sexp, compare]

  let of_char = function
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Tercio
    | 'J' -> Jack
    | 'Q' -> Queen
    | 'K' -> King
    | 'A' -> Ace
    | _ -> failwith "wrong!"
  ;;
end

module Parse = struct
  open Angstrom
  open Util.Parse

  let is_valid_card c =
    let i = Char.(to_int c - to_int '2') in
    (i >= 0 && i <= Char.to_int '9')
    || List.mem [ 'T'; 'J'; 'Q'; 'K'; 'A' ] c ~equal:Char.equal
  ;;

  let card = satisfy is_valid_card >>| Card.of_char
  let hand = [ card; card; card; card; card ] |> list >>| List.sort ~compare:Card.compare

  let hand_bid =
    let* hand = hand in
    let* _ = spaces1 in
    let* bid = integer in
    return (hand, bid)
  ;;

  let puzzle = sep_by1 (many1 endl) (spaces *> hand_bid <* spaces) <* many any_char
  let parse = parse_general puzzle
  let parse_hand = parse_general hand
  let parse_hand_bid = parse_general hand_bid
end

let%expect_test "hand" =
  let hand = "J3T3T" |> Parse.parse_hand in
  printf !"%{sexp:(Card.t List.t)}" hand;
  [%expect "(Three Three Tercio Tercio Jack)"]
;;

let%expect_test "hand" =
  let hand_bid = "J3T3T 868" |> Parse.parse_hand_bid in
  printf !"%{sexp:(Card.t List.t * int)}" hand_bid;
  [%expect "((Three Three Tercio Tercio Jack) 868)"]
;;

let%expect_test "hand" =
  let puzzle = "J3T3T 868\n6Q499 630" |> Parse.parse in
  printf !"%{sexp:((Card.t List.t * int) List.t)}" puzzle;
  [%expect "(((Three Three Tercio Tercio Jack) 868) ((Four Six Nine Nine Queen) 630))"]
;;

let part1 filename =
  let hand_bids =
    let contents = In_channel.read_all filename in
    contents |> Parse.parse
  in
  hand_bids |> sprintf !"%{sexp:((Card.t list * int) list)}\n"
;;
