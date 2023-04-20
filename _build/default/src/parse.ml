module I = Parser.MenhirInterpreter
open Parser
open Types

let info_of_token (token : token) : pre_info =
  match token with
  | WITH i
  | WILDCARD i
  | VAR (i, _)
  | UNIT i
  | TRUE i
  | TO i
  | THEN i
  | SUB i
  | STRING i
  | STR (i, _)
  | SPAWN i
  | SEND i
  | SEMICOLON i
  | SELF i
  | RPAREN i
  | RETURN i
  | REF i
  | RECV i
  | REC i
  | RBRACK i
  | PROMISE i
  | PIPE i
  | OR i
  | NOT i
  | NE i
  | MUL i
  | MOD i
  | MATCH i
  | LT i
  | LPAREN i
  | LIST i
  | LET i
  | LE i
  | LBRACK i
  | INT_LIT (i, _)
  | INT i
  | INCLUDE i
  | IN i
  | IF i
  | HANDLE i
  | TVAR (i, _)
  | GT i
  | GE i
  | FUN i
  | FALSE i
  | EQ i
  | EOF i
  | END i
  | ELSE i
  | DIV i
  | DEREF i
  | CONS i
  | COMMA i
  | COLON i
  | CAT i
  | CASE i
  | BOOL i
  | BIND i
  | BEGIN i
  | AWAIT i
  | ASSIGN i
  | ARROW i
  | AND i
  | ADD i ->
      i

let string_of_token (token : token) : string =
  match token with
  | WITH _ -> "with"
  | WILDCARD _ -> "_"
  | VAR (_, x) -> x
  | UNIT _ -> "unit"
  | TRUE _ -> "true"
  | TO _ -> "to"
  | THEN _ -> "then"
  | SUB _ -> "-"
  | STRING _ -> "string"
  | STR (_, s) -> "\"" ^ String.escaped s ^ "\""
  | SPAWN _ -> "spawn"
  | SEND _ -> "send"
  | SEMICOLON _ -> ";"
  | SELF _ -> "self"
  | RPAREN _ -> ")"
  | RETURN _ -> "return"
  | REF _ -> "ref"
  | RECV _ -> "recv"
  | REC _ -> "rec"
  | RBRACK _ -> "]"
  | PROMISE _ -> "promise"
  | PIPE _ -> "|>"
  | OR _ -> "||"
  | NOT _ -> "not"
  | NE _ -> "<>"
  | MUL _ -> "*"
  | MOD _ -> "%"
  | MATCH _ -> "match"
  | LT _ -> "<"
  | LPAREN _ -> "("
  | LIST _ -> "list"
  | LET _ -> "let"
  | LE _ -> "<="
  | LBRACK _ -> "["
  | INT_LIT (_, n) -> string_of_int n
  | INT _ -> "int"
  | INCLUDE _ -> "include"
  | IN _ -> "in"
  | IF _ -> "if"
  | HANDLE _ -> "handle"
  | TVAR (_, x) -> x
  | GT _ -> ">"
  | GE _ -> ">="
  | FUN _ -> "fun"
  | FALSE _ -> "false"
  | EQ _ -> "="
  | EOF _ -> "eof"
  | END _ -> "end"
  | ELSE _ -> "else"
  | DIV _ -> "/"
  | DEREF _ -> "!"
  | CONS _ -> "::"
  | COMMA _ -> ","
  | COLON _ -> ":"
  | CAT _ -> "^"
  | CASE _ -> "|"
  | BOOL _ -> "bool"
  | BIND _ -> ">>="
  | BEGIN _ -> "begin"
  | AWAIT _ -> "await"
  | ASSIGN _ -> ":="
  | ARROW _ -> "->"
  | AND _ -> "&&"
  | ADD _ -> "+"

(** [parse_error env token lexbuf] is the error message displaying the
    token that crashed the parser. *)
let parse_error (token : token) : string =
  let info = info_of_token token in
  let lstart = string_of_int info.start_lin in
  let lend = string_of_int info.end_lin in
  let cstart = string_of_int info.start_col in
  let cend = string_of_int info.end_col in
  let error_ln1 =
    Printf.sprintf "error in %s at line%s, character%s\n" info.filename
      (if lstart = lend then " " ^ lstart
      else Printf.sprintf "s %s-%s" lstart lend)
      (if cstart = cend then " " ^ cstart
      else Printf.sprintf "s %s-%s" cstart cend)
  in
  let error_ln2 =
    Printf.sprintf "Error: Syntax Error: unexpected token %s\n"
      (string_of_token token)
  in
  error_ln1 ^ error_ln2

(** [parse_error env token lexbuf] is the error message displaying the
    token that crashed the parser. *)
let lex_error (info : pre_info) (msg : string) : string =
  let lstart = string_of_int info.start_lin in
  let lend = string_of_int info.end_lin in
  let cstart = string_of_int info.start_col in
  let cend = string_of_int info.end_col in
  let error_ln1 =
    Printf.sprintf "error in %s at line%s, character%s\n" info.filename
      (if lstart = lend then " " ^ lstart
      else Printf.sprintf "s %s-%s" lstart lend)
      (if cstart = cend then " " ^ cstart
      else Printf.sprintf "s %s-%s" cstart cend)
  in
  let error_ln2 = Printf.sprintf "Error: Lexing Error: %s\n" msg in
  error_ln1 ^ error_ln2

(** [parse_lexbuf lexbuf checkpoint] drives the menhir parser using the
    incremental api. *)
let rec parse_lexbuf
    (lexbuf : Lexing.lexbuf)
    (last_token : token)
    (checkpoint : 'a I.checkpoint)
    (is_file : bool) : ('a, string) result =
  match checkpoint with
  | I.InputNeeded _ -> begin
      try
        let token = Lexer.token lexbuf in
        let startp, endp = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
        let checkpoint = I.offer checkpoint (token, startp, endp) in
        parse_lexbuf lexbuf token checkpoint is_file
      with Lexer.LexingError (i, s) -> Error (lex_error i s)
    end
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse_lexbuf lexbuf last_token checkpoint is_file
  | I.HandlingError _ -> Error (parse_error last_token)
  | I.Accepted v -> Ok v
  | I.Rejected -> Error (parse_error last_token)

let ( >>| ) a b = Stdlib.Result.map b a

(** [parse channel] parses an input channel into an ast. This crashes
    the program on a parsing error. *)
let parse_file (file : string) =
  let ch = open_in file in
  let lexbuf = Lexing.from_channel ~with_positions:true ch in
  (* set filename of the lexbuf *)
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file };
  let ast =
    try
      parse_lexbuf lexbuf (EOF dummy_info)
        (Parser.Incremental.program lexbuf.lex_curr_p)
        true
    with Failure s -> failwith s
  in
  close_in ch;
  ast

let parse_expr expr =
  let lexbuf = Lexing.from_string ~with_positions:true expr in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "" };
  try
    parse_lexbuf lexbuf (EOF dummy_info)
      (Parser.Incremental.top_expr lexbuf.lex_curr_p)
      false
    >>| snd
  with Failure s ->
    print_endline "parse_lexbuf failure";
    failwith s

let parse_prog prog =
  let lexbuf = Lexing.from_string ~with_positions:true prog in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "" };
  try
    parse_lexbuf lexbuf (EOF dummy_info)
      (Parser.Incremental.program lexbuf.lex_curr_p)
      false
  with Failure s ->
    print_endline "parse_lexbuf failure";
    failwith s
