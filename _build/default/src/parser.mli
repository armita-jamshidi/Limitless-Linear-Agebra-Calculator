
(* The type of tokens. *)

type token = 
  | WITH of (Types.pre_info)
  | WILDCARD of (Types.pre_info)
  | VAR of (string Types.info)
  | UNIT of (Types.pre_info)
  | TVAR of (string Types.info)
  | TRUE of (Types.pre_info)
  | TO of (Types.pre_info)
  | THEN of (Types.pre_info)
  | SUB of (Types.pre_info)
  | STRING of (Types.pre_info)
  | STR of (string Types.info)
  | SPAWN of (Types.pre_info)
  | SEND of (Types.pre_info)
  | SEMICOLON of (Types.pre_info)
  | SELF of (Types.pre_info)
  | RPAREN of (Types.pre_info)
  | RETURN of (Types.pre_info)
  | REF of (Types.pre_info)
  | RECV of (Types.pre_info)
  | REC of (Types.pre_info)
  | RBRACK of (Types.pre_info)
  | PROMISE of (Types.pre_info)
  | PIPE of (Types.pre_info)
  | OR of (Types.pre_info)
  | NOT of (Types.pre_info)
  | NE of (Types.pre_info)
  | MUL of (Types.pre_info)
  | MOD of (Types.pre_info)
  | MATCH of (Types.pre_info)
  | LT of (Types.pre_info)
  | LPAREN of (Types.pre_info)
  | LIST of (Types.pre_info)
  | LET of (Types.pre_info)
  | LE of (Types.pre_info)
  | LBRACK of (Types.pre_info)
  | INT_LIT of (int Types.info)
  | INT of (Types.pre_info)
  | INCLUDE of (Types.pre_info)
  | IN of (Types.pre_info)
  | IF of (Types.pre_info)
  | HANDLE of (Types.pre_info)
  | GT of (Types.pre_info)
  | GE of (Types.pre_info)
  | FUN of (Types.pre_info)
  | FALSE of (Types.pre_info)
  | EQ of (Types.pre_info)
  | EOF of (Types.pre_info)
  | END of (Types.pre_info)
  | ELSE of (Types.pre_info)
  | DIV of (Types.pre_info)
  | DEREF of (Types.pre_info)
  | CONS of (Types.pre_info)
  | COMMA of (Types.pre_info)
  | COLON of (Types.pre_info)
  | CAT of (Types.pre_info)
  | CASE of (Types.pre_info)
  | BOOL of (Types.pre_info)
  | BIND of (Types.pre_info)
  | BEGIN of (Types.pre_info)
  | AWAIT of (Types.pre_info)
  | ASSIGN of (Types.pre_info)
  | ARROW of (Types.pre_info)
  | AND of (Types.pre_info)
  | ADD of (Types.pre_info)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top_expr: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.expr Types.info)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.prog)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val top_expr: Lexing.position -> (Types.expr Types.info) MenhirInterpreter.checkpoint
  
  val program: Lexing.position -> (Types.prog) MenhirInterpreter.checkpoint
  
end
