open Types
open Stdlib.Result
open Ast_factory

exception TypeError of string

type typ =
  | TUnit
  | TInt
  | TBool
  | TString
  | THandle
  | TVar of string (* Type variables, like `'a` *)
  | EVar of string
  (* Existential variables, introduced in type inference *)
  | TRef of typ
  | TPromise of typ
  | TList of typ
  | TProd of typ * typ
  | TFun of typ * typ
  | TPoly of typ * typ
  (* Polymorphic function types *)

type pre_error =
  | UnboundVariable of string
  | UnboundTVar of string
  | ExpectedFound of typ * typ

type error = pre_error info

let sprintf = Printf.sprintf

let rec str_of_typ = function
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | THandle -> sprintf "handle"
  | TVar s -> sprintf "%s" s
  | EVar s -> sprintf "%s" s
  | TRef (TProd (x, y)) ->
      sprintf "(%s) ref" (str_of_typ (TProd (x, y)))
  | TRef (TFun (x, y)) -> sprintf "(%s) ref" (str_of_typ (TFun (x, y)))
  | TRef t -> sprintf "%s ref" (str_of_typ t)
  | TPromise (TProd (x, y)) ->
      sprintf "(%s) promise" (str_of_typ (TProd (x, y)))
  | TPromise (TFun (x, y)) ->
      sprintf "(%s) promise" (str_of_typ (TFun (x, y)))
  | TPromise t -> sprintf "%s promise" (str_of_typ t)
  | TList (TProd (x, y)) ->
      sprintf "(%s) list" (str_of_typ (TProd (x, y)))
  | TList (TFun (x, y)) ->
      sprintf "(%s) list" (str_of_typ (TFun (x, y)))
  | TList t -> sprintf "%s list" (str_of_typ t)
  | TProd (TProd (w, x), TProd (y, z)) ->
      sprintf "(%s) * (%s)"
        (str_of_typ (TProd (w, x)))
        (str_of_typ (TProd (y, z)))
  | TProd (TProd (x, y), r) ->
      sprintf "(%s) * %s" (str_of_typ (TProd (x, y))) (str_of_typ r)
  | TProd (l, TProd (x, y)) ->
      sprintf "%s * (%s)" (str_of_typ l) (str_of_typ (TProd (x, y)))
  | TProd (l, r) -> sprintf "%s * %s" (str_of_typ l) (str_of_typ r)
  | TFun (TFun (x, y), r) ->
      sprintf "(%s) -> %s" (str_of_typ (TFun (x, y))) (str_of_typ r)
  | TFun (i, r) -> sprintf "%s -> %s" (str_of_typ i) (str_of_typ r)
  | TPoly (t1, t2) ->
      sprintf "forall _, (%s) -> (%s)" (str_of_typ t1) (str_of_typ t2)

let rec str_of_pat = function
  | PUnit -> sprintf "()"
  | PWild -> sprintf "_"
  | PBool b -> sprintf "%b" (snd b)
  | PInt i -> sprintf "%i" (snd i)
  | PString s -> sprintf "%s" (snd s)
  | PVar _ -> sprintf "[Var]"
  | PPair (p1, p2) ->
      sprintf "(%s, %s)" (str_of_pat (snd p1)) (str_of_pat (snd p2))
  | PNil -> sprintf "[]"
  | PCons (p1, p2) ->
      sprintf "(%s) :: (%s)" (str_of_pat (snd p1)) (str_of_pat (snd p2))

let str_of_info (i : pre_info) : string =
  let lstart = string_of_int i.start_lin in
  let lend = string_of_int i.end_lin in
  let cstart = string_of_int i.start_col in
  let cend = string_of_int i.end_col in
  sprintf "error in %s at line%s, character%s\n" i.filename
    (if lstart = lend then " " ^ lstart
    else sprintf "s %s-%s" lstart lend)
    (if cstart = cend then " " ^ cstart
    else sprintf "s %s-%s" cstart cend)

let unbound_msg (i : pre_info) (v : string) : string =
  sprintf "%sError: Unbound value %s" (str_of_info i) v

let unbound_tv_msg (i : pre_info) (tv : string) : string =
  sprintf
    "%sError: Type variable %s can only appear in polymorphic type"
    (str_of_info i) tv

let typing_msg (i : pre_info) (exp : typ) (fnd : typ) : string =
  sprintf
    "%sError: This expression has type %s, but an expression was \
     expected of type %s\n"
    (str_of_info i) (str_of_typ fnd) (str_of_typ exp)

let polyeq_msg (i : pre_info) (t : typ) : string =
  sprintf
    "%sError: This expression has type %s, but an expression was \
     expected of type string, int, or bool\n"
    (str_of_info i) (str_of_typ t)

let str_of_error (i, e) =
  match e with
  | UnboundVariable v -> unbound_msg i v
  | UnboundTVar tv -> unbound_tv_msg i tv
  | ExpectedFound (expected, found) -> typing_msg i expected found

let ( let* ) = bind

(** Generate an EVar with a fresh name. *)
let fresh_evar =
  let counter = ref 0 in
  fun () ->
    incr counter;
    EVar ("#" ^ string_of_int !counter)

let rec typ_of_gtyp (t : gtyp info) : typ =
  match snd t with
  | GUnit -> TUnit
  | GInt -> TInt
  | GBool -> TBool
  | GString -> TString
  | GHandle -> THandle
  | GTVar (_, s) -> TVar s
  | GRef t -> TRef (typ_of_gtyp t)
  | GPromise t -> TPromise (typ_of_gtyp t)
  | GList t -> TList (typ_of_gtyp t)
  | GProd (t1, t2) -> TProd (typ_of_gtyp t1, typ_of_gtyp t2)
  | GFun (t1, t2) -> TFun (typ_of_gtyp t1, typ_of_gtyp t2)
  | GPoly (t1, t2) -> TPoly (typ_of_gtyp t1, typ_of_gtyp t2)
  | GBot -> fresh_evar ()

(** Get a list of all type variables appearing free in a type. *)
let rec tvars_of_typ (t : typ) : string list =
  begin
    begin
      match t with
      | TVar s -> [ s ]
      | TRef t | TPromise t | TList t -> tvars_of_typ t
      | TProd (t1, t2) | TFun (t1, t2) ->
          tvars_of_typ t1 @ tvars_of_typ t2
      | TPoly (t1, t2) ->
          [] (* No free type variables in a polymorphic type *)
      | _ -> []
    end
    |> List.sort_uniq Stdlib.compare
  end

(** Substitute the type [subst] for the EVar [ev] in type [typ]. *)
let rec subst_evar (subst : typ) (ev : string) (typ : typ) : typ =
  match typ with
  | EVar s when s = ev -> subst
  | TRef t -> TRef (subst_evar subst ev t)
  | TPromise t -> TPromise (subst_evar subst ev t)
  | TList t -> TList (subst_evar subst ev t)
  | TProd (t1, t2) ->
      TProd (subst_evar subst ev t1, subst_evar subst ev t2)
  | TFun (t1, t2) ->
      TFun (subst_evar subst ev t1, subst_evar subst ev t2)
  | _ -> typ

(** Check if EVar [ev] appears in type [typ]. *)
let rec occurs_in (ev : string) (typ : typ) : bool =
  match typ with
  | EVar s when s = ev -> true
  | TRef t | TPromise t | TList t -> occurs_in ev t
  | TProd (t1, t2) | TFun (t1, t2) -> occurs_in ev t1 || occurs_in ev t2
  | _ -> false

(** Instantiate universal type [typ] by replacing type variables with
    fresh existential variables.

    The result is a non-polymorphic type, and the EVars represent
    unknown types that are to be solved for later. If not universal
    type, return input type. *)
let inst_ty (typ : typ) : typ =
  let rec inst bnds = function
    | TVar s -> List.assoc s bnds
    | TRef t -> TRef (inst bnds t)
    | TPromise t -> TPromise (inst bnds t)
    | TList t -> TList (inst bnds t)
    | TProd (t1, t2) -> TProd (inst bnds t1, inst bnds t2)
    | TFun (t1, t2) -> TFun (inst bnds t1, inst bnds t2)
    | t -> t
  in
  match typ with
  | TPoly (t1, t2) ->
      let bindings =
        List.map
          (fun tv -> (tv, fresh_evar ()))
          (tvars_of_typ t1 @ tvars_of_typ t2)
      in
      TFun (inst bindings t1, inst bindings t2)
  | _ -> typ

(** A simple unifier.

    Given a list of equations between types with EVars, a unifier will
    either solve the system of equations---describing how to set every
    EVar so that each equation holds---or report that no solution
    exists. *)
module Unify = struct
  type t = (string * typ) list

  (** An association list representing the current solution.

      Each EVar is mapped to a type, which might mention other EVars.

      RI: EVars that appear in the domain of [solution] do not appear in
      any types in the range of the [solution]. *)
  let solution : t ref = ref []

  (** Clear the current solution. *)
  let reset () : unit = solution := []

  (** Expand an EVar to its current solution, or return it unchanged. *)
  let expand_evar (ev : string) : typ =
    match List.assoc_opt ev !solution with
    | Some t -> t
    | None -> EVar ev

  (** Add an equation [t1] = [t2] to the system of equations.

      Returns [true] if a solution exists, otherwise [false].

      This is a classical algorithm by Martelli and Montanari (1982). At
      a high level, we maintain a worklist of equations to add,
      initially [t1] = [t2]. Then, we remove an equation from the
      worklist and process it, possibly adding more equations and
      updating the current solution, or failing if we find that a
      solution is not possible. If the worklist is empty, then a
      solution exists. Read more here:

      https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm *)
  let unify (t1 : typ) (t2 : typ) : bool =
    let rec unify' to_unify =
      match to_unify with
      | (t, t') :: rest -> begin
          match (t, t') with
          | TUnit, TUnit
          | TInt, TInt
          | TBool, TBool
          | TString, TString
          | THandle, THandle ->
              unify' rest
          | t, t' when t = t' -> unify' rest
          | EVar ev, s | s, EVar ev -> (
              let cur_sol = !solution in
              match List.assoc_opt ev cur_sol with
              (* If [ev] maps to [ty] in current solution, unify [s] and
                 [ty]. *)
              | Some ty -> unify' ((ty, subst_evar ty ev s) :: rest)
              (* Otherwise, substitute [ev] in current solution with [s]
                 and substitute current solution into [s], and extend
                 current solution. *)
              | None ->
                  (* Substitute current solution into [s], call result
                     [s']. *)
                  let s' =
                    List.fold_left
                      (fun accs (ev, sol_ty) ->
                        subst_evar sol_ty ev accs)
                      s cur_sol
                  in
                  (* Substitute [s] for [ev] in current solution, call
                     result [new_sol]. *)
                  let new_sol =
                    List.map
                      (fun (ev', sol_ty) ->
                        (ev', subst_evar s' ev sol_ty))
                      cur_sol
                  in
                  (* If an evar [ev] maps to a type containing [ev],
                     then a solution is not possible. This is the
                     "occurs check". *)
                  if
                    occurs_in ev s'
                    || List.exists
                         (fun (ev, sol_ty) -> occurs_in ev sol_ty)
                         new_sol
                  then false
                  else (
                    solution := (ev, s') :: new_sol;
                    unify' rest))
          | TRef t, TRef t'
          | TPromise t, TPromise t'
          | TList t, TList t' ->
              (* Unify the inner types. *)
              unify' ((t, t') :: rest)
          | TProd (s, t), TProd (s', t') | TFun (s, t), TFun (s', t') ->
              (* Unify both pairs of inner types. *)
              unify' ((s, s') :: (t, t') :: rest)
          (* Should never need to unify two polymorphic types *)
          | TPoly (_, _), TPoly (_, _) -> assert false
          | _ -> false
        end
      | [] -> true
    in
    unify' [ (t1, t2) ]
end

let try_unify (t1 : typ) (t2 : typ) (err : error) : (unit, error) result
    =
  if Unify.unify t1 t2 then ok () else error err

module Context = struct
  type t = (string info * typ) list

  let update (v : string info) (t : typ) (ctxt : t) : t = (v, t) :: ctxt
  let prepend = ( @ )

  let rec find v ctxt =
    match ctxt with
    | [] -> error (fst v, UnboundVariable (snd v))
    | (a, b) :: t -> if snd a = snd v then ok b else find v t

  (* Built-in primitives and their types. *)
  let empty =
    [
      ((Types.dummy_info, "print"), TPoly (TVar "'_", TUnit));
      ((Types.dummy_info, "println"), TPoly (TVar "'_", TUnit));
      ((Types.dummy_info, "int_of_string"), TFun (TString, TInt));
      ((Types.dummy_info, "string_of_int"), TFun (TInt, TString));
    ]
end

(** Find the type of expressions matched by a given pattern [pat].

    Returns error if the pattern is not well-typed. For wildcard and
    variable patterns, the type of expressions matched mentions new
    EVars. *)
let rec type_of_pattern (p : pat info) : (typ * Ast.pat, error) result =
  match snd p with
  | PUnit -> ok (TUnit, make_unit_pattern ())
  | PWild -> ok (fresh_evar (), make_wild_pattern ())
  | PBool (_, b) -> ok (TBool, make_bool_pattern b)
  | PInt (_, n) -> ok (TInt, make_int_pattern n)
  | PString (_, s) -> ok (TString, make_string_pattern s)
  | PVar (_, x) -> ok (fresh_evar (), make_var_pattern x)
  | PPair (p1, p2) -> type_of_pair_pattern p1 p2
  | PNil -> ok (TList (fresh_evar ()), make_nil_pattern ())
  | PCons (p1, p2) -> type_of_cons_pattern p1 p2

and type_of_pair_pattern (p1 : pat info) (p2 : pat info) :
    (typ * Ast.pat, error) result =
  let* t1, p1' = type_of_pattern p1 in
  let* t2, p2' = type_of_pattern p2 in
  ok (TProd (t1, t2), make_pair_pattern p1' p2')

and type_of_cons_pattern (p1 : pat info) (p2 : pat info) :
    (typ * Ast.pat, error) result =
  let* t1, p1' = type_of_pattern p1 in
  let* t2, p2' = type_of_pattern p2 in
  let* _ =
    try_unify (TList t1) t2 (fst p2, ExpectedFound (TList t1, t2))
  in
  ok (TList t1, make_cons_pattern p1' p2')

(** Given a pattern [pat] of type [t], produce a context mapping
    variables introduced in the pattern to types.

    Returns error if the pattern is not well-typed. *)
let rec check_pattern (p : pat info) (t : typ) :
    (Context.t * Ast.pat, error) result =
  let* pt, p' = type_of_pattern p in
  let* _ = try_unify pt t (fst p, ExpectedFound (t, pt)) in
  match (snd p, pt) with
  | PUnit, _
  | PBool _, _
  | PInt _, _
  | PString _, _
  | PNil, _
  | PWild, _ ->
      ok (Context.empty, p')
  (* Try to expand EVar to a more concrete type. *)
  | PVar x, EVar ev -> ok ([ (x, Unify.expand_evar ev) ], p')
  | PPair (p1, p2), TProd (t1, t2) ->
      let* ctxt1, _ = check_pattern p1 t1 in
      let* ctxt2, _ = check_pattern p2 t2 in
      ok (ctxt2 @ ctxt1, p')
  | PCons (p1, p2), TList t' ->
      let* ctxt1, _ = check_pattern p1 t' in
      let* ctxt2, _ = check_pattern p2 t  in
      ok (ctxt2 @ ctxt1, p')
  | _ -> assert false
(* Unreachable *)

(** Typecheck a RML program.

    There are two main features of the type system. First, at top-level
    let- and let-rec definitions, the typechecker automatically
    generalize function types that mention type variables into
    polymorphic types if the definition satisfies the "value
    restriction". Second, when applying a polymorphic type function to
    an argument, the type-checker attempts to "instantiate" the type
    variables with concrete types so that the program typechecks. The
    choice of instantiation might not be known immediately, so the
    typechecker defers the choice by introducing fresh existential
    variables (EVars) and adding constraints to the unifier describing
    which types should be equal. If typechecking finishes and there is a
    solution to the EVars, then typechecking succeeds. If unification
    fails, then typechecking fails because there is no solution to the
    EVars leading to a well-typed program.

    This type system is called the Hindley-Milner type system, and the
    type-checking algorithm is called Algorithm J. Read more here:

    https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system *)
let rec check_prog (ctxt : Context.t) (prog : prog) :
    (Context.t * Ast.prog, error) result =
  let ds = snd prog in
  let h acc d =
    let* accctxt, ds = acc in
    let* accctxt', d' = check_defn accctxt (snd d) in
    ok (accctxt', d' :: ds)
  in
  let* ctxt', ds' = List.fold_left h (ok (ctxt, [])) ds in
  ok (ctxt', List.rev ds')

and check_defn (ctxt : Context.t) (d : defn) :
    (Context.t * Ast.defn, error) result =
  match d with
  | DLet ((_, (p, t)), e) -> check_let_defn ctxt p t e
  | DLetRec ((_, (p, t)), e) -> check_let_rec_defn ctxt p t e

and check_let_defn
    (ctxt : Context.t)
    (p : pat info)
    (gt : gtyp info)
    (e : expr info) : (Context.t * Ast.defn, error) result =
  Unify.reset ();
  let t = typ_of_gtyp gt in
  if tvars_of_typ t <> [] then
    error (fst gt, UnboundTVar (List.nth (tvars_of_typ t) 0))
  else
    let* ctxt', p' = check_pattern p t in
    let* t', e' = check_expr ctxt (snd e) in
    (* Check body of let-def against non-polymorphic type. *)
    let t_inst = inst_ty t in
    let* _ = try_unify t' t_inst (fst e, ExpectedFound (t_inst, t')) in
    ok (ctxt' @ ctxt, make_let_defn p' e')

and check_let_rec_defn
    (ctxt : Context.t)
    (p : pat info)
    (gt : gtyp info)
    (e : expr info) : (Context.t * Ast.defn, error) result =
  Unify.reset ();
  let t = typ_of_gtyp gt in
  if tvars_of_typ t <> [] then
    error (fst gt, UnboundTVar (List.nth (tvars_of_typ t) 0))
  else
    let t_poly = typ_of_gtyp gt in
    let t_inst = inst_ty t_poly in
    (* Inside body of definition, [p] has monomorphic type. Prohibits
       polymorphic recursion, which introduces challenges with
       type-checking: we are using first-order unification, which is
       decidable, but typechecking polymorphic recursion requires
       higher-order unification, which is undecidable. *)
    let* ctxt_inst', _ = check_pattern p t_inst in
    let* t', e' = check_expr (ctxt_inst' @ ctxt) (snd e) in
    let* _ = try_unify t' t_inst (fst e, ExpectedFound (t_poly, t')) in

    (* Outside definition, [p] has polymorphic type. *)
    let* ctxt', p' = check_pattern p t_poly in
    ok (ctxt' @ ctxt, make_let_rec_defn p' e')

and check_expr (ctxt : Context.t) (e : expr) :
    (typ * Ast.expr, error) result =
  match e with
  | Unit -> ok (TUnit, make_unit ())
  | Bool (_, b) -> ok (TBool, make_bool b)
  | Pair (e1, e2) -> check_pair ctxt e1 e2
  | Int (_, n) -> ok (TInt, make_int n)
  | String (_, s) -> ok (TString, make_string s)
  | Self -> ok (THandle, make_self ())
  | Var v -> check_var ctxt v
  | Fun ((_, (p, t)), e) -> check_fun ctxt p t e
  | App (e1, e2) -> check_app ctxt e1 e2
  | Let ((_, (p, t)), e1, e2) -> check_let ctxt p t e1 e2
  | LetRec ((_, (p, t)), e1, e2) -> check_letrec ctxt p t e1 e2
  | Nil -> ok (TList (fresh_evar ()), make_nil ())
  | Bop (b, e1, e2) -> check_bop ctxt b e1 e2
  | Uop (u, e) -> check_uop ctxt u e
  | Seq (e1, e2) -> check_seq ctxt e1 e2
  | IfElse (e1, e2, e3) -> check_if ctxt e1 e2 e3
  | Match (e, cases) -> check_match ctxt e cases
  | Await ((_, (p, t)), e1, e2) -> check_await ctxt p t e1 e2
  | Spawn (e1, e2) -> check_spawn ctxt e1 e2
  | Send (e1, e2) -> check_send ctxt e1 e2
  | Recv e -> check_recv ctxt e
  | Return e -> check_return ctxt e

and check_pair (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
    (typ * Ast.expr, error) result =
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* t2, e2' = check_expr ctxt (snd e2) in
  ok (TProd (t1, t2), make_pair e1' e2')

and check_var (ctxt : Context.t) (v : string info) :
    (typ * Ast.expr, error) result =
  let* t1 = Context.find v ctxt in
  ok (inst_ty t1, make_var (snd v))

and check_fun
    (ctxt : Context.t)
    (p : pat info)
    (t : gtyp info)
    (e : expr info) : (typ * Ast.expr, error) result =
  let* ctxt', p' = check_pattern p (typ_of_gtyp t) in
  let* t', e' = check_expr (ctxt' @ ctxt) (snd e) in
  ok (TFun (typ_of_gtyp t, t'), make_fun p' e')

and check_app (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
    (typ * Ast.expr, error) result =
  let* t, e1' = check_expr ctxt (snd e1) in
  let* t', e2' = check_expr ctxt (snd e2) in
  match t with
  | TFun (t1, t2) ->
      let* _ = try_unify t1 t' (fst e2, ExpectedFound (t1, t')) in
      ok (t2, make_app e1' e2')
  | TPoly _ -> failwith "Bug: found polymorphic type in [check_app]."
  | t -> error (fst e1, ExpectedFound (TFun (t', fresh_evar ()), t))

and check_let
    (ctxt : Context.t)
    (p : pat info)
    (t : gtyp info)
    (e1 : expr info)
    (e2 : expr info) : (typ * Ast.expr, error) result =
  let t = typ_of_gtyp t in
  let* ctxt', p' = check_pattern p t in
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* _ = try_unify t1 t (fst e1, ExpectedFound (t, t1)) in
  let* t2, e2' = check_expr (ctxt' @ ctxt) (snd e2) in
  ok (t2, make_let p' e1' e2')

and check_letrec
    (ctxt : Context.t)
    (p : pat info)
    (t : gtyp info)
    (e1 : expr info)
    (e2 : expr info) : (typ * Ast.expr, error) result =
  let t = typ_of_gtyp t in
  let* ctxt', p' = check_pattern p t in
  let* t1, e1' = check_expr (ctxt' @ ctxt) (snd e1) in
  let* _ = try_unify t1 t (fst e1, ExpectedFound (t, t1)) in
  let* t2, e2' = check_expr (ctxt' @ ctxt) (snd e2) in
  ok (t2, make_let_rec p' e1' e2')

and check_bop
    (ctxt : Context.t)
    (b : bop info)
    (e1 : expr info)
    (e2 : expr info) : (typ * Ast.expr, error) result =
  let expected (input : typ) ~(ret : typ) :
      (typ * Ast.expr, error) result =
    let* t1, e1' = check_expr ctxt (snd e1) in
    let* t2, e2' = check_expr ctxt (snd e2) in
    let* _ = try_unify t1 input (fst e1, ExpectedFound (input, t1)) in
    let* _ = try_unify t2 input (fst e2, ExpectedFound (input, t2)) in
    ok (ret, make_bop (snd b) e1' e2')
  in
  match snd b with
  | Add | Sub | Mul | Div | Mod -> expected TInt ~ret:TInt
  | And | Or -> expected TBool ~ret:TBool
  | Lt | Le | Gt | Ge -> expected TInt ~ret:TBool
  | Eq -> check_eq ctxt true e1 e2
  | Ne -> check_eq ctxt false e1 e2
  | Cat -> expected TString ~ret:TString
  | Pipe -> check_app ctxt e2 e1
  | Cons -> check_cons ctxt e1 e2
  | Assign -> check_assign ctxt e1 e2
  | Bind -> check_bind ctxt e1 e2

and check_eq
    (ctxt : Context.t)
    (eq : bool)
    (e1 : expr info)
    (e2 : expr info) : (typ * Ast.expr, error) result =
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* t2, e2' = check_expr ctxt (snd e2) in
  let* _ = try_unify t1 t2 (fst e2, ExpectedFound (t1, t2)) in
  ok (TBool, make_bop (if eq then Eq else Ne) e1' e2')

and check_cons (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
    (typ * Ast.expr, error) result =
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* t2, e2' = check_expr ctxt (snd e2) in
  match t2 with
  | TList t2' ->
      let* _ = try_unify t1 t2' (fst e1, ExpectedFound (t2', t1)) in
      ok (TList t1, make_bop Cons e1' e2')
  | _ -> error (fst e2, ExpectedFound (TList t1, t2))

and check_assign (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
    (typ * Ast.expr, error) result =
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* t2, e2' = check_expr ctxt (snd e2) in
  match t1 with
  | TRef t1' ->
      let* _ = try_unify t1' t2 (fst e2, ExpectedFound (t1', t2)) in
      ok (TUnit, make_bop Assign e1' e2')
  | _ -> error (fst e1, ExpectedFound (TRef t2, t1))

and check_bind (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
    (typ * Ast.expr, error) result =
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* t2, e2' = check_expr ctxt (snd e2) in
  match t1, t2 with
  | TPromise t1', TFun (t2a, TPromise t2r) ->
      let* _ =
        try_unify t1' t2a (fst e1, ExpectedFound (TPromise t2a, t1))
      in
      ok (TPromise t2r, make_bop Bind e1' e2')
  | _, TPoly _ ->
      failwith "Bug: found polymorphic type in [check_bind]."
  | TPromise t1', _ ->
      error
        ( fst e2,
          ExpectedFound (TFun (t1', TPromise (fresh_evar ())), t2) )
  | _, _ -> error (fst e1, ExpectedFound (TPromise (fresh_evar ()), t1))

and check_uop (ctxt : Context.t) (u : uop info) (e : expr info) :
    (typ * Ast.expr, error) result =
  let expected (input : typ) ~(ret : typ) :
      (typ * Ast.expr, error) result =
    let* t, e' = check_expr ctxt (snd e) in
    let* _ = try_unify t input (fst e, ExpectedFound (input, t)) in
    ok (ret, make_uop (snd u) e')
  in
  match snd u with
  | Neg -> expected TInt ~ret:TInt
  | Not -> expected TBool ~ret:TBool
  | Ref -> check_ref ctxt e
  | Deref -> check_deref ctxt e

and check_ref (ctxt : Context.t) (e : expr info) :
    (typ * Ast.expr, error) result =
  let* t, e' = check_expr ctxt (snd e) in
  ok (TRef t, make_uop Ref e')

and check_deref (ctxt : Context.t) (e : expr info) :
    (typ * Ast.expr, error) result =
  let* t, e' = check_expr ctxt (snd e) in
  match t with
  | TRef t' -> ok (t', make_uop Deref e')
  | _ -> error (fst e, ExpectedFound (TRef (fresh_evar ()), t))

and check_seq (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
    (typ * Ast.expr, error) result =
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* _ = try_unify t1 TUnit (fst e1, ExpectedFound (TUnit, t1)) in
  let* t2, e2' = check_expr ctxt (snd e2) in
  ok (t2, make_seq e1' e2')

and check_if
    (ctxt : Context.t)
    (e1 : expr info)
    (e2 : expr info)
    (e3 : expr info) : (typ * Ast.expr, error) result =
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* _ = try_unify t1 TBool (fst e1, ExpectedFound (TBool, t1)) in
  let* t2, e2' = check_expr ctxt (snd e2) in
  let* t3, e3' = check_expr ctxt (snd e3) in
  let* _ = try_unify t2 t3 (fst e3, ExpectedFound (t2, t3)) in
  ok (t2, make_ifelse e1' e2' e3')

and check_match
    (ctxt : Context.t)
    (e : expr info)
    (cases : (pat info * expr info) list) :
    (typ * Ast.expr, error) result =
  let* t, e' = check_expr ctxt (snd e) in
  let h acct (pi, (i, ei)) =
    let* acct, acccs = acct in
    let* ctxt', p' = check_pattern pi t in
    let* ti, e' = check_expr (ctxt' @ ctxt) ei in
    let* _ = try_unify acct ti (i, ExpectedFound (acct, ti)) in
    ok (ti, (p', e') :: acccs)
  in
  let* ft, cs = List.fold_left h (ok (fresh_evar (), [])) cases in
  ok (ft, make_match e' (List.rev cs))

and check_await
    (ctxt : Context.t)
    (p : pat info)
    (t : gtyp info)
    (e1 : expr info)
    (e2 : expr info) : (typ * Ast.expr, error) result =
  let t = typ_of_gtyp t in
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* ctxt', p' = check_pattern p t in
  let* t2, e2' = check_expr (ctxt' @ ctxt) (snd e2) in
  match (t1, t2) with
  | TPromise t1', TPromise t2' ->
      let* _ = try_unify t1' t (fst e1, ExpectedFound (t1', t)) in
      ok (TPromise t2', make_await p' e1' e2')
  | TPromise _, _ ->
      error (fst e2, ExpectedFound (TPromise (fresh_evar ()), t2))
  | _ -> error (fst e1, ExpectedFound (TPromise (fresh_evar ()), t1))

and check_spawn (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
    (typ * Ast.expr, error) result =
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* t2, e2' = check_expr ctxt (snd e2) in
  match t1 with
  | TFun (t, _) ->
      let* _ = try_unify t2 t (fst e2, ExpectedFound (t, t2)) in
      ok (THandle, make_spawn e1' e2')
  | TPoly _ -> failwith "Bug: found polymorphic type in [check_spawn]."
  | _ -> error (fst e1, ExpectedFound (TFun (t2, fresh_evar ()), t1))

and check_send (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
    (typ * Ast.expr, error) result =
  let* t1, e1' = check_expr ctxt (snd e1) in
  let* t2, e2' = check_expr ctxt (snd e2) in
  match t2 with
  | THandle ->
      let* _ =
        try_unify t1 TString (fst e1, ExpectedFound (TString, t1))
      in
      ok (TUnit, make_send e1' e2')
  | _ -> error (fst e2, ExpectedFound (THandle, t2))

and check_recv (ctxt : Context.t) (e : expr info) :
    (typ * Ast.expr, error) result =
  let* t, e' = check_expr ctxt (snd e) in
  match t with
  | THandle -> ok (TPromise TString, make_recv e')
  | _ -> error (fst e, ExpectedFound (THandle, t))

and check_return (ctxt : Context.t) (e : expr info) :
    (typ * Ast.expr, error) result =
  let* t, e' = check_expr ctxt (snd e) in
  ok (TPromise t, make_return e')
