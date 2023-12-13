open Core
open Stdio

module Parse = struct
  open Angstrom
  open Util.Parse

  let numbers = integers
  let puzzle = many lenient_endl *> sep_by1 lenient_endl numbers <* many any_char
  let parse = parse_general puzzle
end

let next_level xs =
  let seq = Sequence.of_list xs in
  Sequence.zip seq @@ Sequence.drop seq 1
  (* expecting input in reverse, then reversed again by zip above *)
  |> Sequence.map ~f:(fun (nxt, prv) -> nxt - prv)
  |> Sequence.to_list
;;

let layers ~dir xs =
  let xs =
    match dir with
    | `Forward -> List.rev xs
    | `Backward -> xs
  in
  let rec aux acc xs =
    let xs' = next_level xs in
    if List.for_all xs' ~f:(fun ele -> ele = 0) then xs' :: acc else aux (xs' :: acc) xs'
  in
  aux [ xs ] xs
;;

let extrapolate ?(dir = `Forward) xs =
  let xs = layers ~dir xs in
  let rec loop acc prv_ele xs =
    match xs with
    | [] -> acc
    | cur_row :: xs ->
      let cur_ele = prv_ele + List.hd_exn cur_row in
      let acc = cur_ele :: acc in
      loop acc cur_ele xs
  in
  match xs with
  | _ :: _ ->
    let acc = loop [] 0 xs in
    List.hd_exn acc
  | _ -> failwith "nooo!"
;;

let solve ~dir filename =
  let puzzle = In_channel.read_all filename |> Parse.parse in
  puzzle
  |> List.map ~f:(extrapolate ~dir)
  |> List.sum (module Int) ~f:Util.id
  |> sprintf !"%{sexp:(int)}\n"
;;

let part1 = solve ~dir:`Forward
let part2 = solve ~dir:`Backward
