open Core
open Stdio

(* Problem construction *)

module Tile = struct
  type t =
    | Empty
    | Rock
    | Grain

  let empty = function
    | Empty -> true
    | _ -> false
  ;;
end

let fill_in_range arr dim ~lo ~hi =
  let rec aux arr ~lo ~hi =
    if lo > hi
    then ()
    else (
      match dim with
      | `Horizontal i ->
        arr.(i).(lo) <- Tile.Rock;
        aux arr ~lo:(lo + 1) ~hi
      | `Vertical j ->
        arr.(lo).(j) <- Tile.Rock;
        aux arr ~lo:(lo + 1) ~hi)
  in
  let lo, hi = if lo <= hi then lo, hi else hi, lo in
  aux arr ~lo ~hi
;;

let fill_in_pair arr = function
  | (i, aj), (i', bj) when Int.(i = i') -> fill_in_range arr (`Horizontal i) ~lo:aj ~hi:bj
  | (ai, j), (bi, j') when Int.(j = j') -> fill_in_range arr (`Vertical j) ~lo:ai ~hi:bi
  | _ -> failwith "Invalid path"
;;

let fill_in arr path = List.iter path ~f:(fill_in_pair arr)

let read_puzzle filename =
  let f init =
    let f ic =
      let module Step = Sequence.Step in
      match In_channel.input_line ic with
      | None -> Step.Done
      | Some line ->
        (match Parse.path_of_string line with
         | Some value -> Step.Yield { value; state = ic }
         | None -> Step.Skip { state = ic })
    in
    let paths = Sequence.unfold_step ~init ~f |> Sequence.memoize in
    let max_i =
      Sequence.fold paths ~init:0 ~f:(fun acc pairs ->
        Int.max acc
        @@ List.fold pairs ~init:0 ~f:(fun acc ((a, _), (b, _)) ->
          List.max_elt [ a; b; acc ] ~compare:Int.compare |> Option.value_exn))
    in
    let grid = Array.make_matrix Tile.Empty ~dimx:(max_i + 2) ~dimy:1200 in
    Sequence.iter paths ~f:(fill_in grid);
    grid
  in
  In_channel.with_file filename ~f
;;

(* Problem solving *)

type move_grain_t =
  | Cessation of int * int
  | Abyss of int

let rec move_grain_bottomless grid (i, j) ~nrows ~ncols =
  if i + 1 >= nrows
  then Abyss j
  else (
    let next =
      List.find_map
        [ i + 1, j; i + 1, j - 1; i + 1, j + 1 ]
        ~f:(fun (i, j) -> if Tile.empty grid.(i).(j) then Some (i, j) else None)
    in
    match next with
    | None -> Cessation (i, j)
    | Some coords -> move_grain_bottomless grid coords ~nrows ~ncols)

and move_grain_bottomful grid (i, j) ~nrows ~ncols =
  match move_grain_bottomless grid (i, j) ~nrows ~ncols with
  | Cessation (i, j) -> i, j
  | Abyss j -> nrows - 1, j
;;

let solve loop filename =
  let grid = read_puzzle filename in
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
  loop 0 ~grid ~nrows ~ncols
;;

let rec loop1 n ~grid ~nrows ~ncols =
  match move_grain_bottomless grid (0, 500) ~nrows ~ncols with
  | Abyss _ -> n
  | Cessation (i, j) ->
    grid.(i).(j) <- Tile.Grain;
    loop1 (n + 1) ~grid ~nrows ~ncols
;;

let rec loop2 n ~grid ~nrows ~ncols =
  let i, j = move_grain_bottomful grid (0, 500) ~nrows ~ncols in
  grid.(i).(j) <- Tile.Grain;
  if not @@ Tile.empty grid.(0).(500) then n + 1 else loop2 (n + 1) ~grid ~nrows ~ncols
;;

let part1 = solve loop1
let part2 = solve loop2
