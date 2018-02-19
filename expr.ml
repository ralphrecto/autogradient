(* const types *)
type famous_const = E | Pi
type const = IntConst of int
  | FamousConst of famous_const

type expr =
  (* base cases *)
  Const of const
  | Var of string
  (* combinators *)
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Expr of expr * expr (* base, exponent *)

let () =
  print_int 3
