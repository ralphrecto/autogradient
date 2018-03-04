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

(* Convert an expr with any tag into an expr with unit tags. *)
val unitize : 'a t -> unit t

(* Math operators over exprs. *)
val ( + ) : 'a t -> 'a t -> unit t
val ( * ) : 'a t -> 'a t -> unit t
val ( - ) : 'a t -> 'a t -> unit t
val ( / ) : 'a t -> 'a t -> unit t
val ( ^ ) : 'a t -> 'a t -> unit t
val neg : 'a t -> unit t

(* Converter functions *)
val int_of : int -> unit t
val float_of : float -> unit t
val famous_const_of : famous_const -> unit t
val var_of : string -> unit t

val string_of_const : const -> string
val string_of_binop : binop -> string
val string_of_expr : 'a t -> string
