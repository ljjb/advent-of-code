open Core

type monkey_state_t =
  { monkey : Monkey.t
  ; inspections : int
  }

type throw_t =
  { item : int
  ; target : int
  }

type state_t = monkey_state_t array

let update_monkey_state ?(modulus = None) monkey_state =
  let open Monkey in
  let monkey = monkey_state.monkey in
  let new_inspections, throws =
    List.fold_map ~init:0 monkey.items ~f:(fun acc item ->
      let item =
        match monkey.operation with
        | Multiply x -> item * x
        | Add x -> item + x
        | Square -> item * item
      in
      let item =
        match modulus with
        | None -> Int.(item / 3)
        | Some m -> item mod m
      in
      let target =
        match item mod monkey.test.denominator with
        | 0 -> monkey.test.true_target
        | _ -> monkey.test.false_target
      in
      acc + 1, { item; target })
  in
  let monkey = { monkey_state.monkey with items = [] } in
  let inspections = monkey_state.inspections + new_inspections in
  { monkey; inspections }, throws
;;

let give_item monkey item =
  let open Monkey in
  let backwards = List.rev monkey.items in
  let items = List.rev (item :: backwards) in
  { monkey with items }
;;

let update_state ?(modulus = None) state =
  let f i monkey_state =
    let monkey_state, throws = update_monkey_state monkey_state ~modulus in
    state.(i) <- monkey_state;
    List.iter throws ~f:(fun throw ->
      let target_monkey_state = state.(throw.target) in
      let target_monkey = give_item target_monkey_state.monkey throw.item in
      state.(throw.target) <- { target_monkey_state with monkey = target_monkey })
  in
  Array.iteri state ~f
;;

let solve filename ~n ~use_mod =
  let monkeys =
    Monkey.array_of_filename filename
    |> Array.map ~f:(fun monkey -> { monkey; inspections = 0 })
  in
  let modulus =
    if not use_mod
    then None
    else
      Array.map monkeys ~f:(fun monkey -> monkey.monkey.test.denominator)
      |> Array.fold ~init:1 ~f:Int.( * )
      |> Option.return
  in
  for _ = 1 to n do
    update_state monkeys ~modulus
  done;
  Array.sort monkeys ~compare:(fun s1 s2 -> Int.(s2.inspections - s1.inspections));
  monkeys.(0).inspections * monkeys.(1).inspections
;;
