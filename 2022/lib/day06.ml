open Core
open Stdio
module Step = Sequence.Step

let id x = x

let read_chars init =
  let f ic =
    match In_channel.input_char ic with
    | None ->
      In_channel.close ic;
      Step.Done
    | Some value ->
      if Char.is_alpha value
      then Step.Yield { state = ic; value }
      else Step.Skip { state = ic }
  in
  Sequence.unfold_step ~init ~f
;;

let dequeue_if_full ~n q = if Queue.length q > n then Queue.dequeue q else None

(* Fast implementation *)
let solve_puzzle ?(n = 4) filename =
  let aux ic =
    let q = Queue.create () in
    let set = Hash_set.create (module Char) in
    let f acc c =
      Queue.enqueue q c;
      Hash_set.add set c;
      let acc = acc + 1 in
      match dequeue_if_full ~n q with
      | None -> Continue_or_stop.Continue acc
      | Some dropped_c ->
        (* Manually check whether just-dropped char is still in window
           and if not, then remove it from the set *)
        if Queue.mem q dropped_c ~equal:Char.( = )
        then ()
        else Hash_set.remove set dropped_c;
        if Hash_set.length set = n
        then Continue_or_stop.Stop acc
        else Continue_or_stop.Continue acc
    in
    read_chars ic |> Sequence.fold_until ~init:0 ~f ~finish:id
  in
  In_channel.with_file filename ~f:aux
;;

let part1, part2 = solve_puzzle ~n:4, solve_puzzle ~n:14
