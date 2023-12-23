open Core
open Util

module Machine_part = struct
  type t =
    { xtreme : int
    ; musical : int
    ; aero : int
    ; shiny : int
    }
  [@@deriving sexp, equal, hash]

  let get_attr t = function
    | `Xtreme -> t.xtreme
    | `Musical -> t.musical
    | `Aero -> t.aero
    | `Shiny -> t.shiny
  ;;

  let set_attr t a x =
    match a with
    | `Xtreme -> { t with xtreme = x }
    | `Musical -> { t with musical = x }
    | `Aero -> { t with aero = x }
    | `Shiny -> { t with shiny = x }
  ;;

  let of_alist attrs =
    let t = { xtreme = 0; musical = 0; aero = 0; shiny = 0 } in
    List.fold attrs ~init:t ~f:(fun acc (attr, value) -> set_attr acc attr value)
  ;;

  let rating t = t.xtreme + t.musical + t.aero + t.shiny
end

module Rule = struct
  type t =
    { attribute : [ `Xtreme | `Musical | `Aero | `Shiny ]
    ; condition : [ `Greater | `Less ]
    ; bound : int
    ; destination : [ `Workflow of string | `Verdict of bool ]
    }
  [@@deriving sexp, equal, hash]

  let condition_of_char = function
    | '>' -> `Greater
    | '<' -> `Less
    | _ -> failwith "invalid condition input!"
  ;;

  let attr_of_char = function
    | 'x' -> `Xtreme
    | 'm' -> `Musical
    | 'a' -> `Aero
    | 's' -> `Shiny
    | _ -> failwith "invalid attribute input!"
  ;;

  let destination_opt t ~part =
    let op =
      match t.condition with
      | `Greater -> ( > )
      | `Less -> ( < )
    in
    let attr = Machine_part.get_attr part t.attribute in
    if op attr t.bound then Some t.destination else None
  ;;
end

module Workflow = struct
  type t =
    { rules : Rule.t list
    ; default : [ `Workflow of string | `Verdict of bool ]
    }
  [@@deriving sexp, equal, hash]
end

module Range = struct
  type t = int * int [@@deriving sexp, equal, hash]

  let lo (x, _) = x
  let hi (_, y) = y
  let count (x, y) = if y >= x then y - x + 1 else 0
  let compare_lo (alo, _) (blo, _) = Int.compare alo blo
  let point_in (lo, hi) x = x >= lo && x <= hi
  let range_in ~outer:(olo, ohi) ~inner:(ilo, ihi) = ilo >= olo && ihi <= ohi

  let merge ((alo, ahi) as a) ((blo, bhi) as b) =
    if point_in b alo || point_in b ahi
    then
      if range_in ~outer:b ~inner:a
      then Some b
      else Some (Int.min alo blo, Int.max ahi bhi)
    else if range_in ~outer:a ~inner:b
    then Some a
    else None
  ;;

  let add_to_list bs a =
    let rec aux bs a acc =
      match bs with
      | [] -> a :: acc
      | hd :: tl ->
        (match merge a hd with
         | None -> aux tl a (hd :: acc)
         | Some a' -> aux tl a' acc)
    in
    if List.length bs = 0 then [ a ] else aux bs a [] |> List.rev
  ;;

  let merge_lists xs ys = List.fold ys ~init:xs ~f:(fun acc y -> add_to_list acc y)

  let split ((lo, hi) as t) Rule.{ bound; condition; _ } =
    if point_in t bound
    then (
      match condition with
      | `Greater ->
        ( (if bound + 1 <= hi then [ bound + 1, hi ] else [])
        , if lo <= bound then [ lo, bound ] else [] )
      | `Less ->
        ( (if lo <= bound - 1 then [ lo, bound - 1 ] else [])
        , if bound <= hi then [ bound, hi ] else [] ))
    else (
      match condition with
      | `Greater -> if lo > bound then [ lo, hi ], [] else [], [ lo, hi ]
      | `Less -> if hi < bound then [ lo, hi ], [] else [], [ lo, hi ])
  ;;
end

module Quantum_part = struct
  type t =
    { xtreme_r : Range.t list
    ; musical_r : Range.t list
    ; aero_r : Range.t list
    ; shiny_r : Range.t list
    }
  [@@deriving sexp, equal, hash]

  let init range =
    { xtreme_r = [ range ]
    ; musical_r = [ range ]
    ; aero_r = [ range ]
    ; shiny_r = [ range ]
    }
  ;;

  let get_attr t = function
    | `Xtreme -> t.xtreme_r
    | `Musical -> t.musical_r
    | `Aero -> t.aero_r
    | `Shiny -> t.shiny_r
  ;;

  let set_attr t a x =
    match a with
    | `Xtreme -> { t with xtreme_r = x }
    | `Musical -> { t with musical_r = x }
    | `Aero -> { t with aero_r = x }
    | `Shiny -> { t with shiny_r = x }
  ;;

  let merge_attr x y ~attr =
    let x_ranges, y_ranges = get_attr x attr, get_attr y attr in
    let new_ranges = Range.merge_lists x_ranges y_ranges in
    set_attr x attr new_ranges
  ;;

  let merge_all x y =
    [ `Xtreme; `Musical; `Aero; `Shiny ]
    |> List.fold ~init:x ~f:(fun acc attr -> merge_attr acc y ~attr)
  ;;

  let count t =
    [ `Xtreme; `Musical; `Aero; `Shiny ]
    |> List.map ~f:(get_attr t %> List.sum (module Int) ~f:Range.count)
    |> List.fold ~init:1 ~f:( * )
  ;;

  let split_default t = function
    | `Workflow dest -> 0, [ dest, t ]
    | `Verdict true -> count t, []
    | `Verdict false -> 0, []
  ;;

  let split_rule t rule =
    let attr = rule.Rule.attribute in
    let forwarded, kept =
      get_attr t attr
      |> List.map ~f:(fun range -> Range.split range rule)
      |> List.unzip
      |> Tuple2.map ~f:(List.concat %> List.sort ~compare:Range.compare_lo)
    in
    let tally, forwarded =
      match rule.destination with
      | `Verdict accepted ->
        if accepted then count (set_attr t attr forwarded), [] else 0, []
      | `Workflow dst -> 0, [ dst, set_attr t attr forwarded ]
    in
    let kept =
      match kept with
      | [] -> None
      | kept -> Some (set_attr t attr kept)
    in
    kept, tally, forwarded
  ;;

  let split_workflow t workflow =
    let empty_map = Map.empty (module String) in
    List.fold_until
      workflow.Workflow.rules
      ~init:(t, 0, empty_map)
      ~f:(fun (t, tally, forwarded_acc) rule ->
        let kept, delta, forwarded = split_rule t rule in
        let forwarded_acc =
          List.fold forwarded ~init:forwarded_acc ~f:(fun aways (dst, forwarded_part) ->
            Map.update aways dst ~f:(function
              | None -> forwarded_part
              | Some preexisting ->
                merge_attr preexisting forwarded_part ~attr:rule.Rule.attribute))
        in
        match kept with
        | None -> Continue_or_stop.Stop (tally + delta, forwarded_acc)
        | Some kept -> Continue_or_stop.Continue (kept, tally + delta, forwarded_acc))
      ~finish:(fun (t, acc, forwarded_acc) ->
        let tally, forwarded = split_default t workflow.default in
        let forwarded_acc =
          List.fold
            forwarded
            ~init:forwarded_acc
            ~f:(fun forwarded_acc (dst, forwarded_part) ->
              Map.update forwarded_acc dst ~f:(function
                | None -> forwarded_part
                | Some preexisting -> merge_all preexisting forwarded_part))
        in
        acc + tally, forwarded_acc)
  ;;
end
