open Core.Std
open Graph

(* Helper types for expr. *)
type famous_const = E | Pi
type const =
  Int of int
  | Famous of famous_const

type binop = Add | Sub | Mult | Div | Exp

(* Type defining possible mathematical expressions.
 * 'a is a type that can be used to tag a node with auxillary information. *)
type 'a expr =
  (* base cases *)
  Const of 'a * const
  | Var of 'a * string
  (* combinators *)
  | Binop of 'a * binop * 'a expr * 'a expr

(* string_of functions for expr types. *)
let string_of_const = function
  | Int i -> string_of_int i
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
      "( " ^ left_str ^ " " ^ (string_of_binop op) ^ " " ^ right_str ^ " )"

(* name_tree will give each node in tree a unique string identifier except for Vars that have
 * the same string variable name, which will be named the same. *)
let name_tree (tree: 'a expr) : string expr =
  (* Node naming utilities. *)
  let counter : int ref = ref 0 in
  let fresh_name () : string =
    incr counter;
    "w" ^ (string_of_int !counter) in

  (* name_subtree names the nodes in the given given subtree, holding already seen mappings
   * between variable names and node names in varmap (a map of form variable name -> node name). *)
  let rec name_subtree (varmap: string String.Map.t) (subtree: 'a expr) : string String.Map.t * string expr =
    match subtree with
    | Const (_, c) -> varmap, Const (fresh_name (), c)
    | Var (_, var) -> begin
        match Map.find varmap var with
        | None ->
            let varname = fresh_name () in
            Map.add varmap ~key:var ~data:varname, Var (varname, var)
        | Some varname -> varmap, Var (varname, var)
    end
    | Binop (_, op, left_subtree, right_subtree) ->
        let left_varmap, named_left_subtree = name_subtree varmap left_subtree in
        let right_varmap, named_right_subtree = name_subtree left_varmap right_subtree in
        right_varmap, Binop (fresh_name (), op, named_left_subtree, named_right_subtree) in

  name_subtree String.Map.empty tree |> snd

(* Retrieves name/string tag of a given expr node. *)
let get_name = function
  | Const (name, _) | Var (name, _) | Binop (name, _, _, _) -> name

(* Generates the edges of the computation DAG given a named tree of expressions. A node wi depends on a
 * node wj if wj is used in the definition of wi. Such a dependency induces an edge of form (wj, wi) to be
 * included in the output. In tree terms, each parent depends on each of its children. *)
let generate_dag_edges (named_tree: string expr) : (string * string) list =
  let rec inner (edges: (string * string) list) (named_subtree: string expr) : (string * string) list =
    match named_subtree with
    | Const _ | Var _ -> edges
    | Binop (name, _, left_subtree, right_subtree) ->
        let subtree_edges = inner (inner edges left_subtree) right_subtree in
        (get_name left_subtree, name) :: (get_name right_subtree, name) :: subtree_edges in

  inner [] named_tree

let rec simplify (tree: 'a expr) : unit expr =
  match tree with
  | Const (_, c) -> Const ((), c)
  | Var (_, v) -> Var ((), v)
  | Binop (_, op, left, right) -> begin
    let simple_left = simplify left in
    let simple_right = simplify right in
    match op, simple_left, simple_right with
    (* addition simplifications *)
    | Add, Const (_, Int left_int), Const (_, Int right_int) -> Const ((), Int (left_int + right_int))
    | Add, _, Const (_, Int 0) -> simple_left
    | Add, Const (_, Int 0), _ -> simple_right
    (* subtration simplifications *)
    | Sub, Const (_, Int left_int), Const (_, Int right_int) -> Const ((), Int (left_int - right_int))
    | Sub, _, Const (_, Int 0) -> simple_left
    (* multiplication simplifications *)
    | Mult, Const (_, Int left_int), Const (_, Int right_int) -> Const ((), Int (left_int * right_int))
    | Mult, _, Const (_, Int 1) -> simple_left
    | Mult, Const (_, Int 1), _ -> simple_right
    | Mult, Const (_, Int 0), _
    | Mult, _, Const (_, Int 0) -> Const ((), Int 0)
    (* division simplifications *)
    | Div, _, Const (_, Int 1) -> simple_left
    | Div, Const (_, Int 0), _ -> Const ((), Int 0)
    (* exponentiation simplifications *)
    | Exp, _, Const ((), Int 1) -> simple_left
    | Exp, _, Const ((), Int 0)
    | Exp, Const ((), Int 1), _ -> Const ((), Int 1)
    | Exp, Const ((), Int 0), _ -> Const ((), Int 0)
    | _ -> Binop ((), op, simple_left, simple_right)
  end

let differential (tree: 'a expr) (deriv_var : string) : unit expr =
  let rec inner (subtree: 'a expr) (deriv_var : string) : unit expr =
    match subtree with
    | Const (_, _) -> Const ((), Int 0)
    | Var (_, var) ->
        if var = deriv_var then Const ((), Int 1)
        else Const ((), Int 0)
    (* sum rule *)
    | Binop (_, Add, left, right) ->
        Binop ((), Add, inner left deriv_var, inner right deriv_var)
    | Binop (_, Sub, left, right) ->
        Binop ((), Sub, inner left deriv_var, inner right deriv_var)
    (* product rule *)
    | Binop (_, Mult, left, right) ->
        let left_mult_right_prime = Binop ((), Mult, left, inner right deriv_var) in
        let right_mult_left_prime = Binop ((), Mult, inner left deriv_var, right) in
        Binop ((), Add, left_mult_right_prime, right_mult_left_prime)
    (* quotient rule *)
    | Binop (_, Div, left, right) ->
        let left_mult_right_prime = Binop ((), Mult, left, inner right deriv_var) in
        let right_mult_left_prime = Binop ((), Mult, inner left deriv_var, right) in
        Binop ((), Div,
          Binop ((), Sub, right_mult_left_prime, left_mult_right_prime),
          Binop ((), Exp, right, Const ((), Int 2))
        )
    | Binop (_, Exp, left, right) -> begin
      match left, simplify right with
      (* exponential rule for e^x + chain rule *)
      | Const (_, Famous E), _ ->
          Binop ((), Mult, subtree, inner right deriv_var)
      (* power rule + chain rule *)
      | _, Const (_, Int r) when r <> 0 ->
          Binop ((), Mult,
            Binop ((), Mult, Const ((), Int r), Binop ((), Exp, left, Const ((), Int (r-1)))),
            inner left deriv_var
          )
      | _ -> failwith "Not yet implemented."
    end in

  inner tree deriv_var |> simplify

let sigmoid: unit expr =
  Binop ((),
    Div,
    Const ((), Int 1),
    Binop ((),
      Add,
      Const ((), Int 1),
      Binop ((),
        Exp,
        Const ((), Famous E),
        Var ((), "x")
      )
    )
  )

let () =
  differential sigmoid "x" |> string_of_expr |> print_string
