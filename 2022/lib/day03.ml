open Core
module Step = Sequence.Step

let priority_of_char c =
  let i = Char.to_int c in
  if i >= 97 && i <= 122
  then i - 96
  else if i >= 65 && i <= 90
  then i - 65 + 27
  else failwith "wrong!"
;;

let read_rucksacks ~parse_f filename =
  let init = In_channel.create filename in
  let f ic =
    match In_channel.input_line ic with
    | None ->
      In_channel.close ic;
      Step.Done
    | Some line ->
      (match parse_f line with
       | None -> Step.Skip { state = ic }
       | Some parsed -> Step.Yield { value = parsed; state = ic })
  in
  Sequence.unfold_step ~init ~f
;;

let parse_rucksack line =
  line
  |> String.to_list
  |> List.map ~f:priority_of_char
  |> Set.of_list (module Int)
  |> Option.return
;;

let parse_rucksack_compartments line =
  try
    let half_len = String.length line / 2 in
    let lft = line |> String.sub ~pos:0 ~len:half_len |> parse_rucksack in
    let rgt = line |> String.sub ~pos:half_len ~len:half_len |> parse_rucksack in
    Option.map2 lft rgt ~f:(fun x y -> x, y)
  with
  | _ -> None
;;

let read_rucksack_chunks ~parse_f ~n filename =
  let rucksack_stream = read_rucksacks ~parse_f filename in
  Sequence.chunks_exn rucksack_stream n
;;

let badge_of_rucksack (lft, rgt) =
  Set.inter lft rgt |> Set.find ~f:(fun _ -> true) |> Option.value ~default:0
;;

let badge_of_rucksacks r =
  let fold_lst = function
    | [] -> Set.empty (module Int)
    | h :: t -> List.fold ~init:h ~f:(fun acc x -> Set.inter acc x) t
  in
  r |> fold_lst |> Set.find ~f:(fun _ -> true) |> Option.value ~default:0
;;

let find_badges ~parse_f ~find_f filename =
  let rucksack_stream = read_rucksacks ~parse_f filename in
  Sequence.map rucksack_stream ~f:find_f |> Sequence.sum (module Int) ~f:(fun x -> x)
;;

let sum_int_seq = Sequence.sum (module Int) ~f:(fun x -> x)

let part1 filename =
  read_rucksacks ~parse_f:parse_rucksack_compartments filename
  |> Sequence.map ~f:badge_of_rucksack
  |> sum_int_seq
;;

let part2 filename =
  read_rucksack_chunks ~parse_f:parse_rucksack ~n:3 filename
  |> Sequence.map ~f:badge_of_rucksacks
  |> sum_int_seq
;;
