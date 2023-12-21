open Core
open Stdio
open Util

module Operation = struct
  type t =
    | Unembox
    | Embox of int
  [@@deriving sexp]
end

module Instruction = struct
  type t =
    { label_string : string
    ; op : Operation.t
    }
  [@@deriving sexp]
end

module Lens = struct
  type t =
    { box : int
    ; label : string
    ; focal_length : int
    }
  [@@deriving sexp, compare, hash]

  let has_label t ~label = String.equal t.label label
  let have_same_label x y = String.equal x.label y.label
end

let puzzle_hash cs =
  let f acc c = (Char.to_int c + acc) * 17 mod 256 in
  String.to_list cs |> List.fold ~init:0 ~f
;;

module Parse = struct
  open Angstrom
  open Util.Parse

  let non_comma = satisfy (fun c -> not @@ Char.(equal c '\n' || equal c ','))
  let non_comma_string = many @@ non_comma >>| String.of_list
  let csv = sep_by (char ',') non_comma_string <* many @@ char '\n'
  let unembox = char '=' *> integer >>| fun i -> Operation.Embox i
  let embox = char '-' >>| fun _ -> Operation.Unembox
  let operation = choice [ embox; unembox ]
  let label = many1 @@ satisfy Char.is_alpha

  let lens =
    label
    >>= fun label ->
    operation >>= fun op -> return Instruction.{ label_string = String.of_list label; op }
  ;;

  let lenses = sep_by (char ',') lens <* many @@ char '\n' <* many any_char
  let part1 = parse_general (csv >>| List.map ~f:puzzle_hash)
  let part2 = parse_general lenses
end

let part1 =
  In_channel.read_all
  %> Parse.part1
  %> List.fold ~init:0 ~f:( + )
  %> sprintf !"%{sexp:(int)}"
;;

let replace_or_prepend l new_ele ~equal =
  let i_opt =
    List.find_mapi l ~f:(fun i ele -> if equal ele new_ele then Some i else None)
  in
  match i_opt with
  | None -> new_ele :: l
  | Some i_match -> List.mapi l ~f:(fun i ele -> if i = i_match then new_ele else ele)
;;

let part2 filename =
  let instructions = In_channel.read_all filename |> Parse.part2 in
  let boxes = Array.init 256 ~f:(fun _ -> []) in
  List.iter instructions ~f:(fun { label_string = label; op } ->
    match op with
    | Unembox ->
      let lens_box = puzzle_hash label in
      boxes.(lens_box) <- List.filter boxes.(lens_box) ~f:(Lens.has_label ~label %> not)
    | Embox focal_length ->
      let lens = Lens.{ label; box = puzzle_hash label; focal_length } in
      boxes.(lens.box)
        <- replace_or_prepend boxes.(lens.box) lens ~equal:Lens.have_same_label);
  Array.foldi boxes ~init:0 ~f:(fun box_i acc contents ->
    List.rev contents
    |> List.foldi ~init:0 ~f:(fun lens_i acc Lens.{ focal_length; _ } ->
      acc + ((box_i + 1) * (lens_i + 1) * focal_length))
    |> ( + ) acc)
  |> sprintf !"%{sexp:(int)}"
;;
