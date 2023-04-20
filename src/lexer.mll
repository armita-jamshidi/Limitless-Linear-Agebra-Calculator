{
  open Parser
  open Types

  exception LexingError of string info


  let info_of_buf lexbuf = 
    let open Lexing in
    let start = lexbuf.lex_start_p in
    let curr = lexbuf.lex_curr_p in {
      filename = curr.pos_fname;
      start_lin = curr.pos_lnum;
      end_lin = curr.pos_lnum;
      start_col = start.pos_cnum - start.pos_bol + 1;
      end_col = curr.pos_cnum - start.pos_bol + 1;
    }

}

let var = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' ''' '_']*
let tvar = ['''] ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' ''' '_']*
let int = ['0'-'9'] ['0'-'9']*
let str = ['"'] [^'\n''"']* ['"']
let ws = [' ' '\t' '\r' '\n']

rule token = parse
  | '\n'        { Lexing.new_line lexbuf; token lexbuf }
  | ws          { token lexbuf }
  | "//"        { sl_comment lexbuf }
  | "/*"        { ml_comment 0 lexbuf }
  | int as n    { INT_LIT (info_of_buf lexbuf, int_of_string n) }
  | str as s    { STR (info_of_buf lexbuf, String.sub s 1 (String.length s - 2)) }
  | "true"      { TRUE (info_of_buf lexbuf) }
  | "false"     { FALSE (info_of_buf lexbuf) }
  | "fun"       { FUN (info_of_buf lexbuf) }
  | "->"        { ARROW (info_of_buf lexbuf) }
  | "let"       { LET (info_of_buf lexbuf) }
  | "rec"       { REC (info_of_buf lexbuf) }
  | "in"        { IN (info_of_buf lexbuf) }
  | "+"         { ADD (info_of_buf lexbuf) }
  | "-"         { SUB (info_of_buf lexbuf) }
  | "*"         { MUL (info_of_buf lexbuf) }
  | "/"         { DIV (info_of_buf lexbuf) }
  | "%"         { MOD (info_of_buf lexbuf) }
  | "&&"        { AND (info_of_buf lexbuf) }
  | "||"        { OR (info_of_buf lexbuf) }
  | "<"         { LT (info_of_buf lexbuf) }
  | "<="        { LE (info_of_buf lexbuf) }
  | ">="        { GE (info_of_buf lexbuf) }
  | ">"         { GT (info_of_buf lexbuf) }
  | "="         { EQ (info_of_buf lexbuf) }
  | "<>"        { NE (info_of_buf lexbuf) }
  | "^"         { CAT (info_of_buf lexbuf) }
  | "|>"        { PIPE (info_of_buf lexbuf) }
  | ":="        { ASSIGN (info_of_buf lexbuf) }
  | ">>="       { BIND (info_of_buf lexbuf) }
  | "not"       { NOT (info_of_buf lexbuf) }
  | "!"         { DEREF (info_of_buf lexbuf) }
  | "ref"       { REF (info_of_buf lexbuf) }
  | "if"        { IF (info_of_buf lexbuf) }
  | "then"      { THEN (info_of_buf lexbuf) }
  | "else"      { ELSE (info_of_buf lexbuf) }
  | "spawn"     { SPAWN (info_of_buf lexbuf) }
  | "match"     { MATCH (info_of_buf lexbuf) }
  | "|"         { CASE (info_of_buf lexbuf) }
  | "with"      { WITH (info_of_buf lexbuf) }
  | "::"        { CONS (info_of_buf lexbuf) }
  | "await"     { AWAIT (info_of_buf lexbuf) }
  | "send"      { SEND (info_of_buf lexbuf) }
  | "to"        { TO (info_of_buf lexbuf) }
  | "recv"      { RECV (info_of_buf lexbuf) }
  | "return"    { RETURN (info_of_buf lexbuf) }
  | "("         { LPAREN (info_of_buf lexbuf) }
  | ")"         { RPAREN (info_of_buf lexbuf) }
  | "["         { LBRACK (info_of_buf lexbuf) }
  | "]"         { RBRACK (info_of_buf lexbuf) }
  | "begin"     { BEGIN (info_of_buf lexbuf) }
  | "end"       { END (info_of_buf lexbuf) }
  | ","         { COMMA (info_of_buf lexbuf) }
  | ";"         { SEMICOLON (info_of_buf lexbuf) }
  | "_"         { WILDCARD (info_of_buf lexbuf) }
  | ":"         { COLON (info_of_buf lexbuf) }
  | "int"       { INT (info_of_buf lexbuf) }
  | "bool"      { BOOL (info_of_buf lexbuf) }
  | "string"    { STRING (info_of_buf lexbuf) }
  | "list"      { LIST (info_of_buf lexbuf) }
  | "unit"      { UNIT (info_of_buf lexbuf) }
  | "promise"   { PROMISE (info_of_buf lexbuf) }
  | "handle"    { HANDLE (info_of_buf lexbuf) }
  | "include"   { INCLUDE (info_of_buf lexbuf) }
  | "self"      { SELF (info_of_buf lexbuf) }
  | var as v    { VAR (info_of_buf lexbuf, v) }
  | tvar as tv    { TVAR (info_of_buf lexbuf, tv) }
  | eof         { EOF (info_of_buf lexbuf) }
  | _ as x      { raise (LexingError (info_of_buf lexbuf, "unexpected character " ^ String.make 1 x)) }

and sl_comment = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | eof  { EOF (info_of_buf lexbuf) }
  | _    { sl_comment lexbuf }

and ml_comment depth = parse
  | "/*" { ml_comment (depth + 1) lexbuf }
  | "*/" { if depth = 0 then token lexbuf else ml_comment (depth - 1) lexbuf }
  | '\n' { Lexing.new_line lexbuf; ml_comment depth lexbuf }
  | eof  { EOF (info_of_buf lexbuf) }
  | _    { ml_comment depth lexbuf }
