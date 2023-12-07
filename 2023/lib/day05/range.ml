open Core

type t =
  { src_pos : int
  ; dst_pos : int
  ; len : int
  ; delta : int
  }
[@@deriving sexp]

let make ~src_pos ~dst_pos ~len =
  let delta = dst_pos - src_pos in
  { src_pos; dst_pos; len; delta }
;;

let is_in t ~src = src >= t.src_pos && src < t.src_pos + t.len

let find t ~src =
  if src >= t.src_pos && src < t.src_pos + t.len then Some (src + t.delta) else None
;;

let find_reverse t ~dst =
  let src = dst - t.delta in
  if src >= t.src_pos && src < t.src_pos + t.len then Some src else None
;;
