open Core.Std

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

let () =
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
    ) in
  name_tree sigmoid |> ignore
