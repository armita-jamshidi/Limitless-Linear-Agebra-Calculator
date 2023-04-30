(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, handle, uop, bop) are used by the parser and type-checker.
   You do not want to change them.
 ******************************************************************************)

type id = string
type handle = int

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Cat
  | Pipe
  | Cons
  | Assign
  | Bind

and uop =
  | Neg
  | Not
  | Ref
  | Deref

(******************************************************************************
   [pat] is the type of the AST for patterns. You may implement
   this type however you wish. Look at the formal semantics and think about other
   AST types we have worked with for inspiration.
 ******************************************************************************)

(*a pattern can be a primitive type too - i.e. let 1 = 1*)
type pat =
  | PUnit
  | PWildcard
  | PString of string
  | PInt of int
  | PBool of bool
  | PVariable of id
  | PPairs of pat * pat
  | PEList
  | PNEList of pat * pat

(* | PVariable | PUnit | PWild | PString | PInt | PBool | PTuple | PNil
   | PCons of pat * pat *)

(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   the textbook as inspiration.
 ******************************************************************************)

type expr =
  | EUnit
  | EInt of int
  | EString of string
  | EBool of bool
  | EVariable of id
  | EBop of expr * bop * expr
  | EFunction of pat * expr
  | EFunApp of expr * expr
  | EIfStatements of expr * expr * expr
  | EPairs of expr * expr
  | ELet of pat * expr * expr
  | EEmptyLst
  (* | ENELst of (expr * expr) *)
  | ESeq of (expr * expr)
  | EUnary of (uop * expr)
(* | EBinop of bop * expr * expr | EUnit *)

(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement this type
   however you wish.  There are only two kinds of definition---the let
   definition and the let [rec] definition---so this type can be quite simple.
 ******************************************************************************)
and defn = VariableBinding of pat * expr

(******************************************************************************
   [prog] is the type of the AST for an RML program. You should 
   not need to change it.
 ******************************************************************************)

type prog = defn list
