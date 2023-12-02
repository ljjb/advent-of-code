let read_chunks file =
  let rec read_chunk ic acc =
    match input_line ic with
    | "" -> Result.Ok acc
    | exception End_of_file ->
        close_in ic;
        Result.Error acc
    | str -> (
        match int_of_string str with
        | integer -> read_chunk ic (integer :: acc)
        | exception Failure _ -> Result.Ok acc)
  in
  let rec aux ic =
    match read_chunk ic [] with
    | Result.Error chunk -> Seq.Cons (chunk, fun () -> Seq.Nil)
    | Result.Ok chunk -> Seq.Cons (chunk, fun () -> aux ic)
  in
  let ic = open_in file in
  try fun () -> aux ic
  with e ->
    close_in_noerr ic;
    raise e

let top3 (a, b, c) cur =
  if cur > a then (cur, a, b)
  else if cur > b then (a, cur, b)
  else if cur > c then (a, b, cur)
  else (a, b, c)

let solve filename =
  let a, b, c =
    read_chunks filename
    |> Seq.map (List.fold_left ( + ) 0)
    |> Seq.fold_left top3 (0, 0, 0)
  in
  (a, a + b + c)
