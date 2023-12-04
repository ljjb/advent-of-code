open Core
open Angstrom

let many_space = many @@ char ' '
let digit_string = many_space *> take_while Char.is_digit <* many_space
let arrow_sep = many_space *> string "->" *> many_space >>| fun _ -> ()

(* I kind of get monads! *)
let coord =
  digit_string
  <* char ','
  >>= fun j -> digit_string >>| fun i -> Tuple2.map (i, j) ~f:Int.of_string
;;

(* Sugary version *)
let coord' =
  let* j = digit_string <* char ',' in
  let* i = digit_string in
  return @@ Tuple2.map (i, j) ~f:Int.of_string
;;

let path =
  sep_by arrow_sep coord
  >>| fun points ->
  let len = List.length points - 1 in
  List.zip_exn (List.sub points ~pos:0 ~len) (List.sub points ~pos:1 ~len)
;;

let path_of_string line =
  match parse_string ~consume:Consume.All path line with
  | Ok path -> Some path
  | _ -> None
;;
