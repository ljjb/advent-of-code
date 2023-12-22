open Core

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
    let backward_op =
      match t.condition with
      | `Greater -> ( < )
      | `Less -> ( > )
    in
    let passed = Machine_part.get_attr part t.attribute |> backward_op t.bound in
    if passed then Some t.destination else None
  ;;
end

module Workflow = struct
  type t =
    { rules : Rule.t list
    ; default : [ `Workflow of string | `Verdict of bool ]
    }
  [@@deriving sexp, equal, hash]
end
