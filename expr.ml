type famous_const = E | Pi
type const =
Int of int
| Float of float
| Famous of famous_const

type binop = Add | Sub | Mult | Div | Exp

(* Type defining possible mathematical expressions.
* 'a is a type that can be used to tag a node with auxillary information. *)
type 'a t =
(* base cases *)
Const of 'a * const
| Var of 'a * string
(* combinators *)
| Binop of 'a * binop * 'a t * 'a t

let rec unitize (expr : 'a t): unit t =
  match expr with
  | Const (_, c) -> Const ((), c)
  | Var (_, v) -> Var ((), v)
  | Binop (_, op, l, r) -> Binop ((), op, unitize l, unitize r)

let ( + ) l r = Binop ((), Add, unitize l, unitize r)
let ( - ) l r = Binop ((), Sub, unitize l, unitize r)
let ( * ) l r = Binop ((), Mult, unitize l, unitize r)
let ( / ) l r = Binop ((), Div, unitize l, unitize r)
let ( ^ ) l r = Binop ((), Exp, unitize l, unitize r)

let int_of i = Const ((), Int i)
let float_of f = Const ((), Float f)
let famous_const_of f = Const ((), Famous f)
let var_of varname = Var ((), varname)
let neg e = (int_of (-1)) * e

(* string_of functions for expr types. *)
let string_of_const = function
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Famous E -> "e"
  | Famous Pi -> "pi"

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Exp -> "^"

let rec string_of_expr = function
  | Const (_, c) -> string_of_const c
  | Var (_, var) -> var
  | Binop (_, op, left, right) ->
      let left_str = string_of_expr left in
      let right_str = string_of_expr right in
      Pervasives.( "( " ^ left_str ^ " " ^ (string_of_binop op) ^ " " ^ right_str ^ " )" )
