(** Implements the big-step environment model semantics. Important note: the
    the behavior of the interpreter is undefined on programs that do not pass 
    the type-checker. You are free to evaluate malformed programs in any
    way you see fit. *)

open Ast

(** [value] is the type of RML values *)
type value

(** [env] is an environment, which maps identifiers to values *)
type env

(** [initial_env] is the environment in which evaluation begins.
    It must contain all the external functions defined in the writeup, 
    along with a mapping from the string ["_SELF"] to the handle value [0] *)
val initial_env : env

(** [update_env env x v] is [env] updated with a mapping from [x] to [v]. *)
val update_env : env -> id -> value -> env

(** [prepend_env env1 env2] is [env2] updated with all mappings from [env1]. *)
val prepend_env : env -> env -> env

(** [take_env n env] gets the first [n] bindings from [env], throws if [n] is
    bigger than the length of [env]. *)
val take_env : int -> env -> env

(** [size_env env] gets the number of bindings in [env]. *)
val size_env : env -> int

(** [string_of_value v] is a string representing value [v].
    - If [v] is a unit, that string should be ["()"].
    - If [v] is a bool, that string should be [string_of_bool v]. 
    - If [v] is an int, that string shoild be [string_of_int v].
    - If [v] is a string, that string should be
      ["\"" ^ String.escaped v ^ "\""].
    - If [v] is a function, that string should be ["<function>"].
    - If [v] is a promise, that string should be ["<promise>"].
    - If [v] is a ref, that string should be ["<ref>"].
    - If [v] is a pair [(v1, v2)], that string should be
      ["(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"].
    - If [v] is a list, that string should be ["<list>"].
    - If [v] is a handle, that string should be ["<handle>"]. *)
val string_of_value : value -> string

(** [string_of_env env] is a string representation of [env]. It is up to you 
    how to construct that string; it is to be used by you for the purposes of 
    debugging and will not be used for grading. *)
val string_of_env : env -> string

(** [bind_pattern p v] tries to match [v] with [p]. If successful and bindings 
    [b] are produced, then [b] is returned. Behavior is undefined if the pattern
    [p] does not match the value [v]. *)
val bind_pattern : pat -> value -> env option

(** [eval_expr env e] evaluates [e] under environment [env] and returns the
    resulting value, producing any applicable i/o side effects. *)
val eval_expr : env -> expr -> value

(** [eval_defn env d] evaluates [d] under environment [env] and returns an
    an updated environment with the new mappings defined by [d]. *)
val eval_defn : env -> defn -> env

(** [eval_program env prog] evaluates the the program [p] under the environment 
    [env] and returns the resulting environment. *)
val eval_program : env -> prog -> env
