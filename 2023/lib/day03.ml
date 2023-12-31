open Core
open Stdio

let read_puzzle filename =
  let f ic = Util.read_lines ic |> Sequence.map ~f:String.to_array |> Sequence.to_array in
  In_channel.with_file filename ~f
;;

let is_symbol c = not @@ Char.(is_digit c || equal c '.')
let int_of_digit c = Char.to_int c - Char.to_int '0'

module Int_duple = struct
  module Inner = struct
    type t = Int.t * Int.t [@@deriving sexp, hash]

    let compare (ax, ay) (bx, by) =
      match Int.compare ay by with
      | 0 -> Int.compare ax bx
      | d -> d
    ;;
  end

  include Inner
  include Comparable.Make (Inner)
end

let find_num ~matrix ~ncols i j =
  let rec expand_left j =
    let jm1 = j - 1 in
    if jm1 < 0
    then 0
    else if not @@ Char.is_digit matrix.(i).(jm1)
    then j
    else expand_left (j - 1)
  in
  let rec expand_right j =
    let jp1 = j + 1 in
    if jp1 >= ncols
    then ncols - 1
    else if not @@ Char.is_digit matrix.(i).(jp1)
    then j
    else expand_right (j + 1)
  in
  if Char.is_digit matrix.(i).(j) then Some (expand_left j, expand_right j) else None
;;

let nbors ~nrows ~ncols (i, j) =
  [ -1, -1; 0, -1; 1, -1; -1, 0; 1, 0; -1, 1; 0, 1; 1, 1 ]
  |> Sequence.of_list
  |> Sequence.map ~f:(fun (di, dj) -> i + di, j + dj)
  |> Sequence.filter_map ~f:(fun (i, j) ->
    if i >= 0 && i < nrows && j >= 0 && j < ncols then Some (i, j) else None)
;;

let solve part filename =
  let matrix = read_puzzle filename in
  let nrows = Array.length matrix in
  let ncols = Array.length matrix.(0) in
  let find_num = find_num ~matrix ~ncols in
  let nbors = nbors ~nrows ~ncols in
  let explored = Hashtbl.create (module Int_duple) in
  let idxs =
    Array.mapi matrix ~f:(fun i row ->
      Array.filter_mapi row ~f:(fun j _ ->
        if is_symbol row.(j) then Some (i, j) else None)
      |> Array.to_sequence)
    |> Array.to_sequence
    |> Sequence.concat
  in
  match part with
  | `One ->
    idxs
    |> Sequence.map ~f:(fun (i, j) ->
      nbors (i, j)
      |> Sequence.filter_map ~f:(fun (i, j) ->
        match find_num i j with
        | None -> None
        | Some (lo, hi) ->
          let sublen = hi - lo + 1 in
          if Hashtbl.mem explored (i, lo) || (not @@ Char.is_digit matrix.(i).(j))
          then None
          else (
            ignore @@ Hashtbl.add explored ~key:(i, lo) ~data:0;
            let subrow = Array.sub matrix.(i) ~pos:lo ~len:sublen in
            Some (int_of_string (String.of_array subrow)))))
    |> Sequence.concat
    |> Sequence.sum (module Int) ~f:Util.id
  | `Two ->
    idxs
    |> Sequence.map ~f:(fun (i, j) ->
      Hashtbl.clear explored;
      nbors (i, j)
      |> Sequence.iter ~f:(fun (i, j) ->
        match find_num i j with
        | None -> ()
        | Some (lo, hi) ->
          let sublen = hi - lo + 1 in
          let num =
            Array.sub matrix.(i) ~pos:lo ~len:sublen |> String.of_array |> Int.of_string
          in
          Hashtbl.update explored (i, lo) ~f:(function
            | Some _ -> num
            | None -> num));
      if Hashtbl.length explored = 2
      then
        Hashtbl.to_alist explored |> List.fold ~init:1 ~f:(fun acc (_, num) -> acc * num)
      else 0)
    |> Sequence.sum (module Int) ~f:Util.id
;;

let part1 = solve `One
let part2 = solve `Two
