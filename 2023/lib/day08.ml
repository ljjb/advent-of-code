open Core
open Stdio

module Parse = struct
  open Angstrom

  let spaces = many (char ' ')
  let endl = char '\n'
  let double_endl = endl *> endl
  let lr = choice [ (char 'R' >>| fun _ -> `Right); (char 'L' >>| fun _ -> `Left) ]
  let directions = many1 lr
  let node_label = take_while1 Char.is_alphanum

  let destinations =
    let* left = char '(' *> spaces *> node_label <* spaces in
    let* right = char ',' *> spaces *> node_label <* spaces <* char ')' in
    return (left, right)
  ;;

  let node =
    let* node = spaces *> node_label <* spaces <* char '=' <* spaces in
    let* left, right = destinations in
    return (node, left, right)
  ;;

  let puzzle =
    let* directions = directions in
    let* _ = double_endl in
    let* nodes = sep_by1 (many1 endl) node in
    let* _ = many any_char in
    return (directions, nodes)
  ;;

  let parse_general parser input =
    match parse_string parser input ~consume:Consume.All with
    | Ok parsed -> parsed
    | Error thing -> failwith (sprintf !"Invalid input!%s" thing)
  ;;
end

let parse_puzzle = Parse.(parse_general puzzle)

module Stringtbl = Hashtbl.Make (String)

module Graph = struct
  type t = (string * string) Stringtbl.t [@@deriving sexp]

  let create () = Stringtbl.create ()
  let add t (label, left, right) = Hashtbl.set t ~key:label ~data:(left, right)
  let succ t label = Hashtbl.find_exn t label

  let succ_dir t label ~dir =
    let left, right = succ t label in
    match dir with
    | `Left -> left
    | `Right -> right
  ;;

  let starts t =
    Hashtbl.to_alist t
    |> List.filter_map ~f:(fun (label, _) ->
      if Char.equal 'A' @@ String.get label 2 then Some label else None)
  ;;

  let of_tuple_nodes nodes =
    let t = create () in
    List.iter nodes ~f:(add t);
    t
  ;;

  (* Get infinite sequence of zs where z is a number of steps from the starting
     point where you're at a label ending with 'Z'. This avoids doing the graph traversal
     after the point where it starts to cycle. *)
  let z_seq t start ~directions =
    let zs, n =
      Sequence.cycle_list_exn directions
      |> Sequence.fold_until
           ~init:(0, start, [])
           ~finish:(fun _ -> failwith "never!")
           ~f:(fun (cnt, label, zs) dir ->
             let cnt = cnt + 1 in
             let next_label = succ_dir t label ~dir in
             let zs =
               if Char.equal 'Z' @@ String.get next_label 2 then cnt :: zs else zs
             in
             match zs with
             | hd :: hd' :: hd'' :: _ when hd - hd' = hd' - hd'' ->
               Continue_or_stop.Stop (List.rev zs, hd - hd')
             | _ -> Continue_or_stop.Continue (cnt, next_label, zs))
    in
    let final_z = List.hd_exn zs in
    let z_seq = Sequence.of_list zs in
    let endless_seq =
      Sequence.unfold ~init:final_z ~f:(fun state ->
        let state = state + n in
        Some (state, state))
    in
    Sequence.of_list [ z_seq; endless_seq ] |> Sequence.concat
  ;;
end

let part1 filename =
  let directions, nodes = In_channel.read_all filename |> parse_puzzle in
  let graph = Graph.of_tuple_nodes nodes in
  Sequence.cycle_list_exn directions
  |> Sequence.fold_until
       ~init:(0, "AAA")
       ~finish:(fun (cnt, _) -> cnt)
       ~f:(fun (cnt, label) dir ->
         let next_label = Graph.succ_dir graph label ~dir in
         let cnt = cnt + 1 in
         match next_label with
         | "ZZZ" -> Continue_or_stop.Stop cnt
         | _ -> Continue_or_stop.Continue (cnt, next_label))
  |> sprintf "%d\n"
;;

let argmin arr ~compare =
  Array.foldi arr ~init:(0, Int.max_value) ~f:(fun i (acci, accmin) ele ->
    if compare ele accmin < 0 then i, ele else acci, accmin)
  |> fst
;;

let part2 filename =
  let directions, nodes = In_channel.read_all filename |> parse_puzzle in
  let graph = Graph.of_tuple_nodes nodes in
  let starts = Graph.starts graph in
  let z_vals, z_seqs =
    List.map starts ~f:(fun start ->
      Graph.z_seq graph start ~directions |> Sequence.next |> Option.value_exn)
    |> Array.of_list
    |> Array.unzip
  in
  let rec loop () =
    let i = Int.(argmin z_vals ~compare) in
    let next_val, next_seq = Sequence.next z_seqs.(i) |> Option.value_exn in
    z_vals.(i) <- next_val;
    z_seqs.(i) <- next_seq;
    if Array.for_all z_vals ~f:(fun z_val -> Int.equal z_val next_val)
    then next_val
    else loop ()
  in
  loop () |> sprintf "%d\n"
;;
