(** Builds AST nodes for the parser and type-checker. *)

(*******************************************************************************
   This AST "factory" contains functions that the parser and type-checker call 
   to produce AST nodes.  This design enables the parser and type-checker to be
   ignorant of the names of the constructors of your AST, thus enabling everyone
   in the class to design their own AST type.
   You don't want to change any of the names or types appearing in this
   factory, because then the Menhir parser in `parser.mly` would have to be
   modified along with the type-checker---something you really don't want to do.
 ******************************************************************************)

open Ast

(** [make_let_defn p e] represents the definition [ let p = e ]. *)
val make_let_defn : pat -> expr -> defn

(** [make_let_rec_defn p e] represents the definition [ let rec p = e ]. *)
val make_let_rec_defn : pat -> expr -> defn

(** [make_unit_pattern ()] represents the pattern [ () ]. *)
val make_unit_pattern : unit -> pat

(** [make_wild_pattern ()] represents the pattern [ _ ]. *)
val make_wild_pattern : unit -> pat

(** [make_bool_pattern b] represents the pattern [ b ]. *)
val make_bool_pattern : bool -> pat

(** [make_int_pattern n] represents the pattern [ n ]. *)
val make_int_pattern : int -> pat

(** [make_string_pattern s] represents the pattern [ s ]. *)
val make_string_pattern : string -> pat

(** [make_var_pattern x] represents the pattern [ x ]. *)
val make_var_pattern : string -> pat

(** [make_pair_pattern p1 p2] represents the pattern [ (p1, p2) ]. *)
val make_pair_pattern : pat -> pat -> pat

(** [make_nil_pattern ()] represents the pattern [ [] ]. *)
val make_nil_pattern : unit -> pat

(** [make_cons_pattern p1 p2] represents the pattern [ p1 :: p2 ] *)
val make_cons_pattern : pat -> pat -> pat

(** [make_unit ()] represents the expression [ () ]. *)
val make_unit : unit -> expr

(** [make_bool b] represents the expression [ b ]. *)
val make_bool : bool -> expr

(** [make_pair e1 e2] represents the expression [ (e1, e2) ]. *)
val make_pair : expr -> expr -> expr

(** [make_int n] represents the expression [ n ]. *)
val make_int : int -> expr

(** [make_string s] represents the expression [ s ]. *)
val make_string : string -> expr

(** [make_self ()] represents the expression [ self ]. *)
val make_self : unit -> expr

(** [make_var x] represents the expression [ x ]. *)
val make_var : string -> expr

(** [make_fun p e] represents the expression [ fun p -> e ] *)
val make_fun : pat -> expr -> expr

(** [make_app e1 e2] represents the expression [ e1 e2 ]. *)
val make_app : expr -> expr -> expr

(** [make_let p e1 e2] represents the expression [ let p = e1 in e2 ]. *)
val make_let : pat -> expr -> expr -> expr

(** [make_let_rec p e1 e2] represents the expression [let rec p = e1 in e2 ]. *)
val make_let_rec : pat -> expr -> expr -> expr

(** [make_nil ()] represents the expression [ [] ]. *)
val make_nil : unit -> expr

(** [make_bop b e1 e2] represents the expression [ e1 bop e2 ]. *)
val make_bop : bop -> expr -> expr -> expr

(** [make_uop u e] represents the expression [ u e ]. *)
val make_uop : uop -> expr -> expr

(** [make_seq e1 e2] represents the expression [ e1; e2 ]. *)
val make_seq : expr -> expr -> expr

(** [make_ifelse e1 e2 e3] represents the expression [if e1 then e2 else e3 ]. *)
val make_ifelse : expr -> expr -> expr -> expr

(** [make_match e0 [(p1,e1); ... ; (pn,en)] ] represents the expression 
    [ match e0 with | p1 -> e1 ... | pn -> en end ]. *)
val make_match : expr -> (pat * expr) list -> expr

(** [make_await p e1 e2] represents the expression [await p = e1 in e2 ]. *)
val make_await : pat -> expr -> expr -> expr

(** [make_spawn e1 e2] represents the expression [ spawn e1 with e2 ]. *)
val make_spawn : expr -> expr -> expr

(** [make_send e1 e2] represents the expression [ send e1 to e2 ]. *)
val make_send : expr -> expr -> expr

(** [make_recv e] represents the expression [ recv e ]. *)
val make_recv : expr -> expr

(** [make return e] represents the expression [ return e ]. *)
val make_return : expr -> expr

