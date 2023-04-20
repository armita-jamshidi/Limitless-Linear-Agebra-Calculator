open Types

exception TypeError of string

type typ

type error

module Context : sig
  type t
  val update : string info -> typ -> t -> t
  val prepend : t -> t -> t
  val find : string info -> t -> (typ, error) result
  val empty : t
end

val check_prog : Context.t -> Types.prog -> (Context.t * Ast.prog, error) result

val check_expr : Context.t -> Types.expr -> (typ * Ast.expr, error) result

val str_of_error : error -> string

val check_defn : Context.t -> Types.defn -> (Context.t * Ast.defn, error) result