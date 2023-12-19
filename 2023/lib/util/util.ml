open Core
open Stdio

let id x = x
let ( %> ) f g x = g (f x)

let read_lines ic =
  let f ic =
    match In_channel.input_line ic with
    | Some line -> Some (line, ic)
    | None -> None
  in
  Sequence.unfold ~init:ic ~f
;;

module Parse = struct
  open Angstrom

  let space = char ' '
  let spaces, spaces1 = many space, many1 space
  let endl = end_of_line
  let lenient_endl = spaces *> endl <* spaces
  let integer = take_while1 (fun c -> Char.(is_digit c || equal c '-')) >>| Int.of_string
  let integers = sep_by1 spaces1 integer

  let parse_general parser input =
    match parse_string ~consume:Consume.All parser input with
    | Ok parsed -> parsed
    | _ -> failwith "invalid input!"
  ;;
end

module Iteration = struct
  let windows l size =
    let n = List.length l in
    if n < size then failwith "window larger than list!";
    let rec aux seq i window =
      let open Sequence.Generator in
      if i >= n
      then return ()
      else (
        let ele, seq = Sequence.next seq |> Option.value_exn in
        if Fdeque.length window < size
        then aux seq i (Fdeque.enqueue_back window ele)
        else
          yield window
          >>= fun () ->
          let window = Fdeque.drop_front_exn window in
          let window = Fdeque.enqueue_back window ele in
          aux seq (i + 1) window)
    in
    let seq = Sequence.cycle_list_exn l in
    let window = Fdeque.empty in
    Sequence.Generator.run @@ aux seq 0 window |> Sequence.map ~f:Fdeque.to_array
  ;;
end
