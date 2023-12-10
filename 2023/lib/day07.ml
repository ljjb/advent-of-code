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
    | Ten
    | Joker
    | Queen
    | King
    | Ace
  [@@deriving sexp, compare, equal]

  let of_char = function
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Ten
    | 'J' -> Joker
    | 'Q' -> Queen
    | 'K' -> King
    | 'A' -> Ace
    | _ -> failwith "invalid card char!"
  ;;

  let jokers_are_wild = false
end

module Card_jokerfied = struct
  include Card

  let compare x y =
    match x, y with
    | Joker, Joker -> 0
    | Joker, _ -> -1
    | _, Joker -> 1
    | x, y -> Card.compare x y
  ;;

  let jokers_are_wild = true
end

module type Card_comparable = sig
  val compare : Card.t -> Card.t -> int
  val jokers_are_wild : bool
end

module HandF (Card_compare : Card_comparable) = struct
  module Category = struct
    type t =
      | High_card of Card.t * Card.t * Card.t * Card.t * Card.t
      | One_pair of Card.t * Card.t * Card.t * Card.t
      | Two_pair of Card.t * Card.t * Card.t
      | Three_oak of Card.t * Card.t * Card.t
      | Full_house of Card.t * Card.t
      | Four_oak of Card.t * Card.t
      | Five_oak of Card.t
    [@@deriving sexp]

    let to_int t =
      match t with
      | High_card _ -> 1
      | One_pair _ -> 2
      | Two_pair _ -> 3
      | Three_oak _ -> 4
      | Full_house _ -> 5
      | Four_oak _ -> 6
      | Five_oak _ -> 7
    ;;

    (* Can't use the default impl here *)
    let compare x y = to_int x - to_int y

    let jokerfy = function
      | High_card (c, c', c'', c''', Joker) -> One_pair (c, c', c'', c''')
      | One_pair (c, c', c'', Joker) -> Three_oak (c, c', c'')
      | One_pair (Joker, c, c', c'') -> Three_oak (c, c', c'')
      | Two_pair (c, c', Joker) -> Full_house (c, c')
      | Two_pair (c, Joker, c') -> Four_oak (c, c')
      | Three_oak (c, c', Joker) -> Four_oak (c, c')
      | Three_oak (Joker, c, c') -> Four_oak (c, c')
      | Full_house (c, Joker) -> Five_oak c
      | Full_house (Joker, c) -> Five_oak c
      | Four_oak (c, Joker) -> Five_oak c
      | Four_oak (Joker, c) -> Five_oak c
      | other -> other
    ;;

    let of_list cards =
      let len = List.length cards in
      if not (len = 5)
      then failwith "wrong number of cards!"
      else (
        let buckets =
          List.sort_and_group cards ~compare:(fun x y -> Card_compare.compare y x)
          |> List.sort ~compare:(fun x y ->
            let length_delta = List.length y - List.length x in
            if not (length_delta = 0)
            then length_delta
            else if List.length x = 0
            then 0
            else (
              let xhd, yhd = List.(hd_exn x, hd_exn y) in
              Card_compare.compare yhd xhd))
        in
        let naive =
          match buckets with
          | (c :: _) :: [] -> Five_oak c
          | [ c :: _; [ c' ] ] -> Four_oak (c, c')
          | [ c :: _; [ c'; _ ] ] -> Full_house (c, c')
          | [ c :: _; [ c' ]; [ c'' ] ] -> Three_oak (c, c', c'')
          | [ [ c; _ ]; [ c'; _ ]; [ c'' ] ] -> Two_pair (c, c', c'')
          | [ [ c; _ ]; [ c' ]; [ c'' ]; [ c''' ] ] -> One_pair (c, c', c'', c''')
          | [ [ c ]; [ c' ]; [ c'' ]; [ c''' ]; [ c'''' ] ] ->
            High_card (c, c', c'', c''', c'''')
          | _ -> failwith "wrong number of cards!"
        in
        match Card_compare.jokers_are_wild with
        | false -> naive
        | true -> jokerfy naive)
    ;;
  end

  type t =
    { cards : Card.t list
    ; category : Category.t
    }
  [@@deriving sexp]

  let compare x y =
    let category_delta = Category.compare x.category y.category in
    if not (category_delta = 0)
    then category_delta
    else (
      let rec aux xs ys =
        match xs, ys with
        | xhd :: xtl, yhd :: ytl ->
          let card_delta = Card_compare.compare xhd yhd in
          if not (card_delta = 0) then card_delta else aux xtl ytl
        | [], [] -> 0
        | _ :: _, [] | [], _ :: _ -> failwith "wrong number of cards in hand comparison!"
      in
      aux x.cards y.cards)
  ;;

  let of_list cards =
    let category = Category.of_list cards in
    { cards; category }
  ;;
end

module Parse = struct
  open Angstrom
  open Util.Parse

  let is_valid_card_char c =
    let i = Char.(to_int c - to_int '2') in
    (i >= 0 && i <= Char.(to_int '9' - to_int '2'))
    || List.mem [ 'T'; 'J'; 'Q'; 'K'; 'A' ] c ~equal:Char.equal
  ;;

  let card = satisfy is_valid_card_char >>| Card.of_char
  let hand = [ card; card; card; card; card ] |> list

  let hand_bid =
    let* hand = hand in
    let* _ = spaces1 in
    let* bid = integer in
    return (hand, bid)
  ;;

  let puzzle = sep_by1 (many1 endl) (spaces *> hand_bid <* spaces) <* many any_char
  let parse = parse_general puzzle
end

let solve (module Card_compare : Card_comparable) filename =
  let module Hand = HandF (Card_compare) in
  In_channel.read_all filename
  |> Parse.parse
  |> List.map ~f:(fun (cs, b) -> Hand.of_list cs, b)
  |> List.sort ~compare:(fun (left_hand, _) (right_hand, _) ->
    Hand.compare left_hand right_hand)
  |> List.foldi ~init:0 ~f:(fun i acc (_, bid) -> acc + ((i + 1) * bid))
  |> sprintf !"%{sexp:(int)}\n"
;;

let part1 = solve (module Card)
let part2 = solve (module Card_jokerfied)
