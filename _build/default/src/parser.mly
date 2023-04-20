/* Token Definitions */

%token <int Types.info> INT_LIT
%token <string Types.info> STR
%token <string Types.info> VAR
%token <string Types.info> TVAR
%token <Types.pre_info> TRUE FALSE
%token <Types.pre_info> FUN ARROW
%token <Types.pre_info> AWAIT LET IN WILDCARD REC
%token <Types.pre_info> ADD SUB MUL DIV MOD AND OR LT LE GT GE EQ NE ASSIGN CAT PIPE BIND
%token <Types.pre_info> NOT REF DEREF
%token <Types.pre_info> IF THEN ELSE
%token <Types.pre_info> MATCH CASE CONS
%token <Types.pre_info> SPAWN WITH
%token <Types.pre_info> SEND TO
%token <Types.pre_info> RECV
%token <Types.pre_info> RETURN
%token <Types.pre_info> LPAREN RPAREN LBRACK RBRACK EOF BEGIN END SEMICOLON COMMA COLON
%token <Types.pre_info> INT BOOL STRING LIST UNIT PROMISE HANDLE
%token <Types.pre_info> INCLUDE SELF

/* %type <prog> program */
%type <string Types.info> incl 
%type <defn info> defn
%type <pat info> base_pat pat
%type <var info> typed_pat paren_tpat
%type <pat info * expr info> case 
%type <expr info> app value 
%type <gtyp info> typ 
%type <bop info> bin 
%type <uop info> una

/* Precedence and Associativity */


%right ARROW

%nonassoc IN ELSE
%left PIPE BIND

%left SEMICOLON

%nonassoc ASSIGN

%nonassoc TO WITH RECV

%right RETURN
%right CONS
%left OR
%left AND
%left EQ NE
%left LT LE GE GT
%left ADD SUB
%left MUL DIV MOD
%left LIST PROMISE REF
%left CAT

%right NOT
%right DEREF

%start <Types.prog> program
%start <Types.expr Types.info> top_expr

/* Helper functions */

%{
  open Types

  (** [fold_fun ps e] is the function [fun p1 -> ... -> fun pn -> e]. *)
  let fold_fun ps (e : expr info) : expr info =
    List.fold_right (fun p f -> dummy_info, Fun (p, f)) ps e

  let fold_fun_typ (tvs : var info list) (rt : gtyp info) : gtyp info =
    List.fold_right (fun (_,(_,t)) acc -> dummy_info, GFun (t,acc)) tvs rt

  (** Generalize type if function type, otherwise return unchanged. *)
  let gen_fun_typ (ft : gtyp info) : gtyp info =
    match snd ft with
    | GFun (gt, gt') -> (fst ft, GPoly (gt, gt'))
    | _ -> ft

  (** Generalize type if [pt] is function type and [e] is (function) value,
      otherwise return unchanged.

      Only generalizing the types of values is known as the "value restriction"
      in OCaml. This restriction is needed for soundness in the presence of
      references. For more details, see here:

      https://ocamlverse.github.io/content/weak_type_variables.html *)
  let gen_let_pat_typ (pt : var info) (e : expr info) : var info =
    let (i, (p, t)) = pt in
    match snd e with
    | Fun (_, _) -> (i, (p, gen_fun_typ t))
    | _ -> pt

  (** [defold_list es] desugars List [e1; ... ; en] to [Cons (e1, ... Cons (en, Nil))] *)
  let rec defold_list es =
    match es with
    | [] -> dummy_info, Nil
    | h::t -> dummy_info, Bop ((dummy_info, Cons), h, defold_list t)

  (** [defold_plist ps] desugars PList [p1; ... ; pn] to [PCons (p1, ... Cons (pn, Nil))] *)
  let rec defold_plist ps =
    match ps with
    | [] -> dummy_info, PNil
    | p::t -> dummy_info, PCons (p, defold_plist t)

  let (<+>) i1 i2 = {
    filename = if i1.filename = "" then i2.filename else i1.filename;
    start_lin = i1.start_lin;
    end_lin = i2.end_lin;
    start_col = i1.start_col;
    end_col = i2.end_col;
  }

%}

%%

program:
  | is=incl* ds=defn+ EOF { is,ds }

top_expr:
  | e=exp EOF { e }

incl:
  | INCLUDE s=STR { s }

defn:
  | i=LET l=LPAREN r=RPAREN EQ e=exp
    { let pos = l <+> r in
      i <+> (fst e), DLet ((pos, ((pos, PUnit), (pos, GUnit))), e) }
  | i=LET w=WILDCARD EQ e=exp
    { i <+> (fst e), DLet ((w, ((w, PWild), (w, GBot))), e) }
  | i=LET p=typed_pat EQ e=exp  { i <+> (fst e), DLet (gen_let_pat_typ p e, e) }
  | i=LET p=paren_tpat EQ e=exp { i <+> (fst e), DLet (gen_let_pat_typ p e, e) }
  | i=LET f=VAR tvs=paren_tpat+ COLON t=typ EQ e=exp 
    { i <+> (fst e), DLet ((fst f, ((fst f, PVar f), gen_fun_typ (fold_fun_typ tvs t))), fold_fun tvs e) }
  | i=LET REC f=VAR COLON t=typ EQ e=exp 
    { i <+> (fst e), DLetRec (((fst f) <+> (fst t), ((fst f, PVar f), gen_fun_typ t)),e) }
  | i=LET REC f=VAR tvs=paren_tpat+ COLON t=typ EQ e=exp 
    { i <+> (fst e), DLetRec (((fst f) <+> (fst t), ((fst f, PVar f), gen_fun_typ (fold_fun_typ tvs t))), fold_fun tvs e) }

exp:
  | i=IF e1=exp THEN e2=exp ELSE e3=exp             { i <+> (fst e3), IfElse (e1, e2, e3) }
  | m=MATCH e1=exp WITH cs=case+ e=END              { m <+> e, Match (e1, cs) }
  | f=FUN tv=paren_tpat ARROW e=exp                 { f <+> (fst e), Fun (tv, e) }
  | i=LET l=LPAREN r=RPAREN  EQ e1=exp IN e2=exp
    { let pos = l <+> r in
      (i <+> (fst e2), Let ((pos, ((pos, PUnit), (pos, GUnit))), e1, e2)) }
  | i=LET w=WILDCARD  EQ e1=exp IN e2=exp
    { (i <+> (fst e2), Let ((w, ((w, PWild), (w, GBot))), e1, e2)) }
  | i=LET tv=typed_pat EQ e1=exp IN e2=exp          { i <+> (fst e2), Let (tv, e1, e2) }
  | i=LET tv=paren_tpat EQ e1=exp IN e2=exp         { i <+> (fst e2), Let (tv, e1, e2) }
  | i=LET f=VAR tvs=paren_tpat+ COLON t=typ EQ e1=exp IN e2=exp
    { i <+> (fst e2), 
      Let (((fst f) <+> (fst t), ((fst f, PVar f), fold_fun_typ tvs t)),
          fold_fun tvs e1, e2) }
  | i=AWAIT tv=typed_pat EQ e1=exp IN e2=exp        { i <+> (fst e2), Await (tv, e1, e2) }
  | i=AWAIT tv=paren_tpat EQ e1=exp IN e2=exp       { i <+> (fst e2), Await (tv, e1, e2) }
  | i=SEND e1=exp TO e2=exp                         { i <+> (fst e2), Send (e1, e2) }
  | i=RECV e=exp                                    { i <+> (fst e), Recv e }
  | i=SPAWN e1=exp WITH e2=exp                      { i <+> (fst e2), Spawn (e1, e2) }
  | e1=exp SEMICOLON e2=exp                         { (fst e1) <+> (fst e2), Seq (e1, e2) }
  | e1=exp op=bin e2=exp                            { (fst e1) <+> (fst e2), Bop (op, e1, e2) }
  | op=una e=exp                                    { (fst op) <+> (fst e), Uop (op, e) }
  | i=RETURN e=exp                                  { i <+> (fst e), Return e }
  | i=LET REC f=VAR COLON t=typ EQ e1=exp IN e2=exp
    { i <+> (fst e2), LetRec (((fst f) <+> (fst t), ((fst f, PVar f),t)), e1, e2) }
  | i=LET REC f=VAR tvs=paren_tpat+ COLON t=typ EQ e1=exp IN e2=exp
    { i <+> (fst e2), LetRec (((fst f) <+> (fst t), ((fst f, PVar f),fold_fun_typ tvs t)), fold_fun tvs e1, e2) }
  | a=app                                         { a }

base_pat:
  | i=WILDCARD        { i, PWild }
  | l=LPAREN r=RPAREN { l <+> r, PUnit }
  | i=TRUE            { i, PBool (i, true) }
  | i=FALSE           { i, PBool (i, false) }
  | i=INT_LIT         { fst i, PInt i }
  | u=SUB i=INT_LIT   { u <+> fst i, PInt (fst i, -(snd i)) }
  | s=STR             { fst s, PString s }

pat:
  | b=base_pat                                        { b }
  | s=VAR                                             { fst s, PVar s }
  | l=LPAREN p1=pat COMMA p2=pat r=RPAREN             { l <+> r, PPair (p1, p2) }
  | p1=pat CONS p2=pat                                { (fst p1) <+> (fst p2), PCons (p1, p2) }
  | LBRACK l=separated_list(SEMICOLON, pat) RBRACK    { defold_plist l }


typed_pat:
  | p=pat COLON t=typ { (fst p) <+> (fst t), (p, t) }

paren_tpat:
  | LPAREN p=typed_pat RPAREN { p }

case:
  | CASE p=pat ARROW e=exp { (p, e) }

app:
  | a=app v=value { (fst a) <+> (fst v), App (a, v) }
  | v=value       { v }

value:
  | i=INT_LIT                                           { fst i, Int i }
  | i=SELF                                              { i, Self }
  | s=STR                                               { fst s, String s }
  | s=VAR                                               { fst s, Var s }
  | i=TRUE                                              { i, Bool (i, true) }
  | i=FALSE                                             { i, Bool (i, false) }
  | l=LPAREN r=RPAREN                                   { l <+> r, Unit }
  | l=LPAREN e=exp r=RPAREN                             { l <+> r, snd e }
  | l=LPAREN e1=exp COMMA e2=exp r=RPAREN               { l <+> r, Pair (e1, e2) }
  | l=LBRACK ps=separated_list(SEMICOLON, app) r=RBRACK { l <+> r, snd (defold_list ps) }
  | l=BEGIN e=exp r=END                                 { l <+> r, snd e }
  

typ:
  | i=UNIT              { i, GUnit }
  | i=INT               { i, GInt }
  | i=BOOL              { i, GBool }
  | i=STRING            { i, GString }
  | i=HANDLE            { i, GHandle }
  | s=TVAR                        { fst s, GTVar s }
  | t=typ i=REF         { (fst t) <+> i, GRef t }
  | t=typ i=PROMISE     { (fst t) <+> i, GPromise t}
  | t=typ i=LIST        { (fst t) <+> i, GList t }
  | t1=typ MUL t2=typ   { (fst t1) <+> (fst t2), GProd (t1, t2) }
  | t1=typ ARROW t2=typ { (fst t1) <+> (fst t2), GFun (t1, t2) }
  | LPAREN t=typ RPAREN { t }

%inline bin:
| i=ADD    { i, Ast.Add }
| i=SUB    { i, Ast.Sub }
| i=MUL    { i, Ast.Mul }
| i=DIV    { i, Ast.Div }
| i=MOD    { i, Ast.Mod }
| i=AND    { i, Ast.And }
| i=OR     { i, Ast.Or }
| i=LT     { i, Ast.Lt }
| i=LE     { i, Ast.Le }
| i=GT     { i, Ast.Gt }
| i=GE     { i, Ast.Ge }
| i=EQ     { i, Ast.Eq }
| i=NE     { i, Ast.Ne }
| i=CAT    { i, Ast.Cat }
| i=PIPE   { i, Ast.Pipe }
| i=CONS   { i, Ast.Cons }
| i=ASSIGN { i, Ast.Assign }
| i=BIND   { i, Ast.Bind }

%inline una:
| i=SUB   { i, Ast.Neg }
| i=NOT   { i, Ast.Not }
| i=REF   { i, Ast.Ref }
| i=DEREF { i, Ast.Deref }
