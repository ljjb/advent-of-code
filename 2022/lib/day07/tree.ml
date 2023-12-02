open Core
open Parse

type t =
  { dirname : string
  ; dirsize : int
  ; (* contents : (string, entry_t, String.comparator_witness) Map.t; *)
    contents : entry_t Map.M(String).t
  }
[@@deriving sexp_of]

and entry_t =
  | Leaf of file_t
  | Node of t

let empty_tree dirname = { dirname; dirsize = 0; contents = Map.empty (module String) }

let name_of_entry = function
  | Leaf { filename; _ } -> filename
  | Node { dirname; _ } -> dirname
;;

let rec add_entry tree forward_path to_be_added =
  match forward_path with
  | [] ->
    { tree with
      contents =
        (match
           Map.add tree.contents ~key:(name_of_entry to_be_added) ~data:to_be_added
         with
         | `Ok contents -> contents
         | _ -> tree.contents)
    }
  | hd :: tl ->
    (match Map.find tree.contents hd with
     | None -> tree
     | Some entry ->
       (match entry with
        | Leaf _ -> tree
        | Node sub_tree ->
          let sub_tree = add_entry sub_tree tl to_be_added in
          { tree with contents = Map.set tree.contents ~key:hd ~data:(Node sub_tree) }))
;;

let add_dir tree forward_path dir = add_entry tree forward_path (Node dir)
let add_file tree forward_path file = add_entry tree forward_path (Leaf file)

let get_sub_trees tree =
  Map.filter_map tree.contents ~f:(fun data ->
    match data with
    | Leaf _ -> None
    | Node sub_tree -> Some sub_tree)
;;

let get_sub_tree_list tree =
  let alist = get_sub_trees tree |> Map.to_alist in
  alist |> List.map ~f:(fun (_, v) -> v)
;;

let rec build_sized tree =
  let init = 0, Map.empty (module String) in
  let dirsize, contents =
    Map.fold tree.contents ~init ~f:(fun ~key ~data (size_acc, tree_acc) ->
      let size, sized_entry = build_entry_sized data in
      size + size_acc, Map.set tree_acc ~key ~data:sized_entry)
  in
  { tree with contents; dirsize }

and build_entry_sized entry =
  match entry with
  | Leaf { size; _ } as leaf -> size, leaf
  | Node sub_tree ->
    let ({ dirsize; _ } as tree) = build_sized sub_tree in
    dirsize, Node tree
;;

let get_size tree =
  let { dirsize; _ } = build_sized tree in
  dirsize
;;

let rec traverse_trees tree =
  let root_seq = Sequence.return tree in
  let child_seq =
    get_sub_tree_list tree
    |> Sequence.of_list
    |> Sequence.concat_map ~f:(fun x -> traverse_trees x)
  in
  Sequence.of_list [ root_seq; child_seq ] |> Sequence.concat
;;

let get_trees_matching ~pred tree = traverse_trees tree |> Sequence.filter ~f:pred

let get_trees_gte ~size tree =
  let pred tree = tree.dirsize >= size in
  get_trees_matching ~pred tree
;;

let get_trees_lte ~size tree =
  let pred tree = tree.dirsize <= size in
  get_trees_matching ~pred tree
;;
