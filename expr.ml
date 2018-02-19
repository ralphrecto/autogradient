(* Const helper types for expr. *)
type famous_const = E | Pi
type const = IntConst of int
  | FamousConst of famous_const

(* Type defining possible mathematical expressions.
 * 'a is a type that can be used to tag a node with auxillary information. *)
type 'a expr =
  (* base cases *)
  Const of 'a * const
  | Var of 'a * string
  (* combinators *)
  | Add of 'a * 'a expr * 'a expr
  | Sub of 'a * 'a expr * 'a expr
  | Mult of 'a * 'a expr * 'a expr
  | Div of 'a * 'a expr * 'a expr
  | Exp of 'a * 'a expr * 'a expr (* tag, base, exponent *)

(* TODO: describe node naming scheme. *)
let name_tree (tree: 'a expr) : string expr =
  (* Node naming utilities. *)
  let counter : int ref = ref 0 in
  let fresh_name () : string =
    incr counter;
    "w" ^ (string_of_int !counter) in
  let rec name_subtree (subtree: 'a expr) : string expr =
    match subtree with
    | Const (_, c) -> Const (fresh_name (), c)
    | Var (_, v) -> Var (fresh_name (), v)
    | Add (_, x, y) -> Add (fresh_name (), name_subtree x, name_subtree y)
    | Sub (_, x, y) -> Sub (fresh_name (), name_subtree x, name_subtree y)
    | Mult (_, x, y) -> Mult (fresh_name (), name_subtree x, name_subtree y)
    | Div (_, x, y) -> Div (fresh_name (), name_subtree x, name_subtree y)
    | Exp (_, x, y) -> Exp (fresh_name (), name_subtree x, name_subtree y) in
  name_subtree tree

let () =
  print_int 3
