open Core
module In_channel = Stdio.In_channel
module Step = Sequence.Step

type change_dir_t =
  | Root_dir
  | Up_a_dir
  | Down_a_dir of string
[@@deriving sexp_of]

type cmd_line_t =
  | Cd of change_dir_t
  | Ls
[@@deriving sexp_of]

type file_t =
  { size : int
  ; filename : string
  }
[@@deriving sexp_of]

type output_line_t =
  | File of file_t
  | Dir of string
[@@deriving sexp_of]

type parsed_t =
  | Cmd of cmd_line_t
  | Outputs of output_line_t list
[@@deriving sexp_of]

let ( %> ) g f x = x |> g |> f
let id x = x

let parse_dir = function
  | "/" -> Root_dir
  | ".." -> Up_a_dir
  | dir -> Down_a_dir dir
;;

let parse_cmd = function
  | "cd" :: dir :: _ -> Cd (parse_dir dir)
  | "ls" :: _ -> Ls
  | _ -> failwith "unknown command"
;;

let parse_output_line = function
  | "dir" :: dir :: _ -> Dir dir
  | digits :: filename :: _ -> File { size = Int.of_string digits; filename }
  | _ -> failwith "illegible output"
;;

let parse_line line =
  match line |> String.split ~on:' ' |> List.filter ~f:(String.is_empty %> not) with
  | "$" :: t -> Cmd (parse_cmd t)
  | chunks -> Outputs (parse_output_line chunks :: [])
;;

let read_lines init =
  let f ic = In_channel.input_line ic |> Option.map ~f:(fun x -> x, ic) in
  Sequence.unfold ~init ~f |> Sequence.map ~f:parse_line
;;

let parse_lines = read_lines

module Parse_state = struct
  type t =
    { seq : parsed_t Sequence.t
    ; cmd_lines : cmd_line_t list
    ; output_lines : output_line_t list
    }

  let init seq = { seq; cmd_lines = []; output_lines = [] }
end

type parsed_list = parsed_t list [@@deriving sexp_of]

let parse_and_print filename =
  let f ic =
    let formatter = Format.formatter_of_out_channel Stdio.stdout in
    let sexp = parse_lines ic |> Sequence.to_list |> sexp_of_parsed_list in
    Sexplib.Sexp.pp_hum formatter sexp
  in
  In_channel.with_file filename ~f
;;
