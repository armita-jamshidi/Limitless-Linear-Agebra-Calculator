type pre_info = {
  filename : string;
  start_lin : int;
  end_lin : int;
  start_col : int;
  end_col : int;
}

let dummy_info =
  {
    filename = "";
    start_lin = 0;
    end_lin = 0;
    start_col = 0;
    end_col = 0;
  }

type 'a info = pre_info * 'a

type gtyp =
  | GUnit
  | GInt
  | GBool
  | GString
  | GHandle
  | GTVar of string info
  | GRef of gtyp info
  | GPromise of gtyp info
  | GList of gtyp info
  | GProd of gtyp info * gtyp info
  | GFun of gtyp info * gtyp info
  | GPoly of gtyp info * gtyp info
  | GBot

type prog = string info list * defn info list
and var = pat info * gtyp info

and defn =
  | DLet of var info * expr info
  | DLetRec of var info * expr info

and expr =
  | Unit
  | Bool of bool info
  | Int of int info
  | String of string info
  | Var of string info
  | Self
  | Pair of expr info * expr info
  | Nil
  | Fun of var info * expr info
  | App of expr info * expr info
  | Let of var info * expr info * expr info
  | LetRec of var info * expr info * expr info
  | Bop of bop info * expr info * expr info
  | Uop of uop info * expr info
  | Seq of expr info * expr info
  | IfElse of expr info * expr info * expr info
  | Match of expr info * (pat info * expr info) list
  | Await of var info * expr info * expr info
  | Spawn of expr info * expr info
  | Send of expr info * expr info
  | Recv of expr info
  | Return of expr info

and pat =
  | PUnit
  | PWild
  | PBool of bool info
  | PInt of int info
  | PString of string info
  | PVar of string info
  | PPair of pat info * pat info
  | PNil
  | PCons of pat info * pat info

and bop = Ast.bop
and uop = Ast.uop

(** [exp_of_prog prog] folds a program into a single let expression *)
let exp_of_prog (prog : prog) : expr =
  let f d acc =
    match snd d with
    | DLet (v, e) -> (fst d, Let (v, e, acc))
    | DLetRec (v, e) -> (fst d, LetRec (v, e, acc))
  in
  List.fold_right f (snd prog) (dummy_info, Unit) |> snd
