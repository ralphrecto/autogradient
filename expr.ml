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

module Expr = struct
  let rec unitize (expr : 'a expr): unit expr =
    match expr with
    | Const (_, c) -> Const ((), c)
    | Var (_, v) -> Var ((), v)
    | Binop (_, op, l, r) -> Binop ((), op, unitize l, unitize r)

  let generic_binop (op: binop) (l: 'a expr) (r: 'a expr): unit expr = Binop ((), op, unitize l, unitize r)

  let ( + ) = generic_binop Add
  let ( - ) = generic_binop Sub
  let ( * ) = generic_binop Mult
  let ( / ) = generic_binop Div
  let ( ^ ) = generic_binop Exp

  let int_of i = Const ((), Int i)
  let famous_const_of f = Const ((), Famous f)
  let var_of varname = Var ((), varname)
  let neg e = (int_of (-1)) * e
end

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

(* Simplify the given tree of expressions into simpler expressions using mathematical laws,
 * mostly identity laws under operators. *)
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

(* Given a named expression tree, returns a map of form (node name -> simple form of node).
 * The simple form of a node is the same if it is a Const or a Var. If it is Binop, its operands
 * are rewritten to Vars, where the Var name is the name of each operand's node in the named tree. *)
let get_simple_mappings (named_tree: string expr) : (unit expr) String.Map.t =
  let rec inner (named_subtree: string expr) (exprmap: (unit expr) String.Map.t) : (unit expr) String.Map.t =
    match named_subtree with
    | Const (name, c) -> Map.add exprmap ~key:name ~data:(Const ((), c))
    | Var (name, v) -> Map.add exprmap ~key:name ~data:(Var ((), v))
    | Binop (name, op, left_subtree, right_subtree) -> begin
        let left_name = get_name left_subtree in
        let right_name = get_name right_subtree in
        let simple_binop = Binop ((), op, Var ((), left_name), Var ((), right_name)) in
        exprmap |> inner left_subtree |> inner right_subtree |> Map.add ~key:name ~data:simple_binop
      end in

  inner named_tree String.Map.empty

let get_full_mappings (named_tree: 'a expr) : (unit expr) String.Map.t =
  let rec inner (named_subtree: string expr) (exprmap: (unit expr) String.Map.t) : ((unit expr) String.Map.t) * (unit expr) =
    match named_subtree with
    | Const (name, c) ->
        let unitized = Const ((), c) in
        Map.add exprmap ~key:name ~data:unitized, unitized
    | Var (name, v) ->
        let unitized = Var ((), v) in
        Map.add exprmap ~key:name ~data:unitized, unitized
    | Binop (name, op, left_subtree, right_subtree) -> begin
      let left_map, left_unitized = inner left_subtree exprmap in
      let right_map, right_unitized = inner right_subtree left_map in
      let unitized = Binop ((), op, left_unitized, right_unitized) in
      Map.add right_map ~key:name ~data:unitized, unitized
    end in

  inner named_tree String.Map.empty |> fst

(* Generates the edges of the computation DAG given a named tree of expressions. A node wi depends on a
 * node wj if wj is used in the definition of wi. Such a dependency induces an edge of form (wj, wi) to be
 * included in the output. In tree terms, each parent depends on each of its children. *)
let generate_compute_dag_edges (named_tree: string expr) : (string * string) list =
  let rec inner (edges: (string * string) list) (named_subtree: string expr) : (string * string) list =
    match named_subtree with
    | Const _ | Var _ -> edges
    | Binop (name, _, left_subtree, right_subtree) ->
        let subtree_edges = inner (inner edges left_subtree) right_subtree in
        (get_name left_subtree, name) :: (get_name right_subtree, name) :: subtree_edges in

  inner [] named_tree

module StringCmp = struct
  type t = string
  let compare = String.compare
  let hash = String.hash
  let equal = String.equal
end

module ComputeGraph = Persistent.Digraph.Concrete(StringCmp)

(* Given the named expression tree, computes the computation DAG of the tree. *)
let get_compute_dag (named_tree: string expr) : ComputeGraph.t =
  (* Generate the edges of the compute DAG. *)
  let computed_dag_edges = generate_compute_dag_edges named_tree in

  (* Add the computed edges to a DAG. *)
  let foldf graph (node1, node2) = ComputeGraph.add_edge graph node1 node2 in
  List.fold_left ~init:ComputeGraph.empty ~f:foldf computed_dag_edges

(* Computes a reverse topological sort of the given compute_dag. *)
let reverse_topo_sort (compute_dag: ComputeGraph.t) : string list =
  let module TopoComputeGraph = Topological.Make(ComputeGraph) in
  TopoComputeGraph.fold (fun node nodelist -> node :: nodelist) compute_dag []

let rec simple_differential (simple_mappings: unit expr String.Map.t) (simple_expr: 'a expr) (deriv_var: string) : unit expr =
  match simple_expr with
  | Const _ -> Expr.( int_of 0 )
  | Var (_, var) -> begin
      if var = deriv_var then Expr.( int_of 1 )
      else Expr.( int_of 0 )
  end
  | Binop (_, op, Var (left_tag, left_var), Var (right_tag, right_var)) -> begin
    let left_operand = Var (left_tag, left_var) in
    let right_operand = Var (right_tag, right_var) in
    let left_unitized = Expr.( unitize left_operand ) in
    let right_unitized = Expr.( unitize right_operand ) in
    let left_deriv = simple_differential simple_mappings left_operand deriv_var in
    let right_deriv = simple_differential simple_mappings right_operand deriv_var in
    match op with
    (* sum rules *)
    | Add -> Expr.( left_deriv + right_deriv)
    | Sub -> Expr.( left_deriv - right_deriv)
    (* product rule *)
    | Mult -> Expr.( (left_unitized * right_deriv) + (left_deriv * right_unitized) )
    (* quotient rule *)
    | Div -> Expr.(
      let numerator = (left_deriv * right_unitized) - (left_unitized * right_deriv) in
      let denominator = right_unitized ^ (int_of 2) in
      numerator / denominator )
    | Exp -> begin
      match Map.find simple_mappings left_var, Map.find simple_mappings right_var with
      (* e^x rule *)
      | Some ( Const ((), Famous E) ), Some _ -> Expr.(
        ((famous_const_of E) ^ (right_unitized)) * (simple_differential simple_mappings right_unitized deriv_var) )
      (* power rule *)
      | Some e1, Some ( Const ((), Int n ) ) when n <> 0 ->
        let new_power = n - 1 in
        Expr.( (int_of n) * (e1 ^ ( int_of new_power)) * left_deriv )
      | _ -> failwith "couldn't find mappings"
    end
  end
  | _ -> failwith "fatal state: called simple differential on non-simple expression."

let compute_chain_rule_cmp
  (simple_mappings: (unit expr) String.Map.t)
  (deriv_var: string)
  (sumopt: unit expr option)
  (next_components: (unit expr option) * (unit expr option)) : unit expr option =
  match sumopt, next_components with
  | Some sum, (Some deriv, Some simple_expr) ->
      let differential = simple_differential simple_mappings simple_expr deriv_var in
      let ret = Expr.( (deriv * differential) + sum ) in
      Some (simplify ret)
  | _ -> None

let compute_deriv_foldf
  (compute_dag: ComputeGraph.t)
  (simple_mappings: (unit expr) String.Map.t)
  (deriv_map: (unit expr) String.Map.t)
  (next_var: string) : (unit expr) String.Map.t =
    let out_nodes = ComputeGraph.succ compute_dag next_var in
    let out_nodes_deriv_mapping: unit expr option list = List.map ~f:(Map.find deriv_map) out_nodes in
    let out_nodes_simple_mapping: unit expr option list = List.map ~f:(Map.find simple_mappings) out_nodes in
    match List.zip out_nodes_deriv_mapping out_nodes_simple_mapping with
    | None -> "out nodes of var " ^ next_var ^ " do not all have simple mappings and deriv mappings." |> failwith
    | Some out_nodes_components -> begin
        let foldf = compute_chain_rule_cmp simple_mappings next_var in
        let next_var_deriv_opt = List.fold_left ~f:foldf ~init:(Some (Const ((), Int 0))) out_nodes_components in
        match next_var_deriv_opt with
        | None -> "could not compute partial derivative of " ^ next_var |> failwith
        | Some next_var_deriv -> Map.add deriv_map ~key:next_var ~data:(simplify next_var_deriv)
    end

let compute_derivs (named_tree: string expr): (unit expr) String.Map.t =
  let compute_dag: ComputeGraph.t = get_compute_dag named_tree in
  match reverse_topo_sort compute_dag with
  | [] -> failwith "fatal state: no variables found in the reverse toposort of the compute dag."
  | deriv_var :: rest_of_vars ->
      (* For each var, we are computing the gradient d${deriv_var}/d${var}, which equals 1 when var === deriv_var. *)
      let deriv_map_init = Map.add String.Map.empty ~key:deriv_var ~data:(Const ((), Int 1)) in
      let simple_mappings = get_simple_mappings named_tree in
      List.fold_left ~f:(compute_deriv_foldf compute_dag simple_mappings) ~init:deriv_map_init rest_of_vars

let expand (full_mappings: unit expr String.Map.t) (expr_with_simple_subexpr: unit expr) : unit expr option =
  let rec expand_inner (subexpr: unit expr) : unit expr option =
    match subexpr with
    | Const _ -> Some subexpr
    | Var ((), varname) -> Map.find full_mappings varname |> Option.map ~f:ident
    | Binop ((), op, left, right) -> begin
      let expanded_left_opt = expand_inner left in
      let expanded_right_opt = expand_inner right in
      match expanded_left_opt, expanded_right_opt with
      | Some expanded_left, Some expanded_right ->
          Some ((Binop ((), op, expanded_left, expanded_right)) |> simplify)
      | _ -> None
    end in

  expand_inner expr_with_simple_subexpr

let get_var_names (named_tree: string expr) : string String.Map.t =
  let rec inner (named_subtree: string expr) (mapacc: string String.Map.t) : string String.Map.t =
    match named_subtree with
    | Var (nodename, varname) -> Map.add mapacc ~key:varname ~data:nodename
    | Binop (_, _, left, right) -> mapacc |> inner left |> inner right
    | _ -> mapacc in

  inner named_tree String.Map.empty

let derive_and_expand_vars (named_tree: string expr) : unit expr String.Map.t =
  let derivs = compute_derivs named_tree in
  let var_to_name_map = get_var_names named_tree in
  let full_mappings = get_full_mappings named_tree in

  let foldf ~key:var ~data:varname (mapacc: unit expr String.Map.t) =
    let full_deriv_opt = Option.(
      Map.find derivs varname >>= fun varderiv ->
      expand full_mappings varderiv
    ) in
    match full_deriv_opt with
    | None -> "failed to compute derivative w.r.t. variable " ^ var |> failwith
    | Some full_deriv -> Map.add mapacc ~key:var ~data:full_deriv in

  Map.fold ~init:String.Map.empty ~f:foldf var_to_name_map

let sigmoid (x: unit expr) : unit expr = Expr.(
  (int_of 1) / ((int_of 1) + ((famous_const_of E) ^ (neg x)))
)

type layer = string list

let rec layer (base: string) (num: int) : layer =
  if num < 1 then
    failwith "num must be >= 1."
  else begin
    let newvar = base ^ (string_of_int num) in
    if num = 1 then [ newvar ]
    else newvar :: layer base (num - 1)
  end

let named_layer_exprs (l: layer): (string * unit expr) list =
  List.map ~f:(fun nodename -> (nodename, Expr.( var_of nodename ))) l

let layer_exprs (l: layer): unit expr list =
  List.map ~f:(fun nodename -> Expr.( var_of nodename )) l

let gen_layer_exprs
  (prev_layer_exprs: (string * unit expr) list)
  (next_layer: layer): (string * unit expr) list =
  let gen_weighted_prev_comb (nodename: string): unit expr =
    let weight_linear_comb =
      List.fold_left
      ~init:Expr.(int_of 0)
      ~f:(fun linearcombacc (prevname, prevexpr) ->
        let weight_var = Expr.var_of ("w" ^ prevname ^ "_" ^ nodename) in
        Expr.( linearcombacc + ( weight_var * prevexpr) )
      )
      prev_layer_exprs
      in

    sigmoid weight_linear_comb
  in

  List.map ~f:(fun nodename -> (nodename, gen_weighted_prev_comb nodename)) next_layer

(* the last layer must have the same number of elements as the target layer,
 * otherwise will return None. *)
let network_error_expr (layers: layer list) (target_var_base: string) : unit expr option =
  let foldf (last_layer_exprs_opt: (string * unit expr) list option) (next_layer: layer) =
    match last_layer_exprs_opt with
    (* hit this case when next_layer is the first layer. *)
    | None -> Some (named_layer_exprs next_layer)
    (* hit this case for non-first layers *)
    | Some last_layer_exprs -> Some (gen_layer_exprs last_layer_exprs next_layer) in

  let output_layer_exprs_opt = List.fold_left ~init:None ~f:foldf layers in
  Option.(
    output_layer_exprs_opt >>= fun output_layer_exprs ->
    Some (layer target_var_base (List.length output_layer_exprs)) >>= fun target ->
    List.zip output_layer_exprs (layer_exprs target) >>| fun output_target_pairs ->
    Expr.(
      let sq_error_sum = List.fold_left
        ~init:(int_of 0)
        ~f:(fun errorsum ((_, output), target) -> errorsum + ((target - output) ^ (int_of 2)))
        output_target_pairs in
      sq_error_sum / (int_of 2)
    )
  )

let rec subst (expr: unit expr) (varmap: unit expr String.Map.t) : unit expr =
  match expr with
  | Const _ -> expr
  | Var (_, v) -> begin
    match Map.find varmap v with
    | None -> expr
    | Some var_expr -> var_expr
  end
  | Binop (_, op, left_expr, right_expr) ->
    Binop ((), op, subst left_expr varmap, subst right_expr varmap)

let gen_var_mappings (var_base: string) (var_values: unit expr list) : unit expr String.Map.t =
  let indexed_values =
    List.mapi ~f:(fun index var_value -> (index+1, var_value)) var_values in

  List.fold_left
    ~init:String.Map.empty
    ~f:(fun mapacc (varindex, input_val) ->
      Map.add mapacc ~key:(var_base ^ (string_of_int varindex)) ~data:input_val)
    indexed_values

(* TODO: enable Const floats *)
let backprop
  (network: layer list)
  (input_var_base: string)
  (target_var_base: string)
  (* TODO: enable float inputs/outputs *)
  (input: int list)
  (target: int list) : unit expr String.Map.t option =
    let input_exprs = List.map ~f:Expr.( int_of ) input in
    let target_exprs = List.map ~f:Expr.( int_of ) target in
    let input_mappings = gen_var_mappings input_var_base input_exprs in
    let target_mappings = gen_var_mappings target_var_base target_exprs in
    let merged_mappings = Map.merge
      input_mappings
      target_mappings
      ~f:(fun ~key bindings ->
        match bindings with
        | `Both _ -> failwith "variables for inputs and outputs must be distinct."
        | `Left value | `Right value -> Some value ) in

    Option.(
      network_error_expr network target_var_base >>= fun error_expr ->
      Some (subst error_expr merged_mappings) >>| fun weight_var_error_expr ->
      derive_and_expand_vars (name_tree weight_var_error_expr)
    )

let sigmoid_expr: unit expr = Expr.(
  (int_of 1) / ((int_of 1) + ((famous_const_of E) ^ (neg (var_of "x"))))
)

let expr1 : unit expr = Expr.(
  (int_of 1) / ((famous_const_of E) ^ (neg (var_of "x")))
)

let test_network: layer list = [
  layer "x" 2 ;
  layer "h" 3 ;
  layer "y" 2
]

let print_derivs (derivmap: unit expr String.Map.t) =
  Map.iteri derivmap ~f:(fun ~key ~data -> key ^ ": " ^ (string_of_expr data) |> print_endline)

let () =
  match backprop test_network "x" "t" [ 1; 2 ] [ 4; 5 ] with
  | None -> failwith "could not compute backprop for network."
  | Some derivs -> print_derivs derivs
