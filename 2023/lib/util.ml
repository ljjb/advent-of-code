open Core
open Stdio

let id x = x

let read_lines ic =
  let f ic =
    match In_channel.input_line ic with
    | Some line -> Some (line, ic)
    | None -> None
  in
  Sequence.unfold ~init:ic ~f
