open Core

type t =
  { from_name : string
  ; to_name : string
  ; ranges : Range.t List.t
  }
[@@deriving sexp]

let find t src = List.find_map t.ranges ~f:(Range.find ~src) |> Option.value ~default:src

let find_reverse t dst =
  List.find_map t.ranges ~f:(Range.find_reverse ~dst) |> Option.value ~default:dst
;;

let find_many t xs = List.map xs ~f:(fun x -> find t x)
let find_many_reverse t xs = List.map xs ~f:(fun x -> find_reverse t x)

let map_of_list ts =
  let map = Map.empty (module String) in
  List.fold ts ~init:map ~f:(fun acc t ->
    match Map.add acc ~key:t.from_name ~data:t with
    | `Ok acc' -> acc'
    | `Duplicate -> acc)
;;

let los_of_list r = List.map r ~f:(fun t -> t.Range.src_pos)

let points_of_interest t =
  List.fold ~init:[] t.ranges ~f:(fun acc x -> x.src_pos :: (x.src_pos + x.len) :: acc)
  |> List.dedup_and_sort ~compare:Int.compare
;;

let points_of_interest_transformed t =
  points_of_interest t |> List.map ~f:(fun x -> find t x)
;;
