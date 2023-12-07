open Core

let map_keys =
  [ "seed"; "soil"; "fertilizer"; "water"; "light"; "temperature"; "humidity" ]
;;

let transform xs ~map ~keys =
  List.map xs ~f:(fun x ->
    List.fold keys ~init:x ~f:(fun acc cur ->
      let cur_map = Map.find_exn map cur in
      Range_map.find cur_map acc))
;;

let part1 filename =
  let contents = In_channel.(with_file filename ~f:input_all) in
  let seeds, range_maps = contents |> Parse.parse_puzzle in
  let range_map_map = Range_map.map_of_list range_maps in
  transform seeds ~map:range_map_map ~keys:map_keys
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> sprintf !"%{sexp:(int)}"
;;

let part2 filename =
  let contents = In_channel.(with_file filename ~f:input_all) in
  let seeds, range_maps = contents |> Parse.parse_puzzle in
  let seeds =
    List.chunks_of seeds ~length:2
    |> List.filter_map ~f:(function
      | [ start; len ] -> Some (Range.make ~src_pos:start ~dst_pos:start ~len)
      | _ -> None)
  in
  let range_maps =
    Range_map.{ from_name = "begin"; to_name = "seed"; ranges = seeds } :: range_maps
  in
  let range_map_map = Range_map.map_of_list range_maps in
  let backward_map_keys = List.rev map_keys in
  let ends =
    List.fold
      ("begin" :: map_keys)
      ~init:(Range_map.los_of_list seeds)
      ~f:(fun acc map_key ->
        let cur_map = Map.find_exn range_map_map map_key in
        let points = Range_map.points_of_interest cur_map in
        let points = List.concat [ acc; points ] in
        Range_map.find_many cur_map points)
  in
  let starts =
    List.fold backward_map_keys ~init:ends ~f:(fun acc map_key ->
      let cur_map = Map.find_exn range_map_map map_key in
      Range_map.find_many_reverse cur_map acc)
    |> List.filter ~f:(fun x -> List.exists seeds ~f:(fun s -> Range.is_in s ~src:x))
  in
  transform starts ~map:range_map_map ~keys:map_keys
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> sprintf !"%{sexp:(int)}"
;;
