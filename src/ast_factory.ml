open Ast

let make_let_defn p e = VariableBinding (p, e)
let make_let_rec_defn p e = failwith "Unimplemented: make_let_rec_defn"
let make_unit_pattern () = PUnit
let make_wild_pattern () = PWildcard
let make_bool_pattern b = PBool b
let make_int_pattern n = PInt n
let make_string_pattern s = PString s
let make_var_pattern x = PVariable x
let make_pair_pattern p1 p2 = PPairs (p1, p2)
let make_nil_pattern () = PEList
let make_cons_pattern p1 p2 = PNEList (p1, p2)
let make_unit () = EUnit
let make_bool b = EBool b
let make_pair e1 e2 = EPairs (e1, e2)
let make_int n = EInt n
let make_string s = EString s
let make_self () = failwith "Unimplemented: make_self"
let make_var x = EVariable x
let make_fun p e = EFunction (p, e)
let make_app e1 e2 = EFunApp (e1, e2)
let make_let p e1 e2 = ELet (p, e1, e2)
let make_let_rec p e1 e2 = failwith "Unimplemented: make_let_rec"
let make_nil () = EEmptyLst
let make_bop b e1 e2 = EBop (e1, b, e2)
let make_uop u e = EUnary (u, e)
let make_seq e1 e2 = ESeq (e1, e2)
let make_ifelse e1 e2 e3 = EIfStatements (e1, e2, e3)
let make_match e cs = failwith "Unimplemented: make_match"
let make_await p e1 e2 = failwith "Unimplemented: make_await"
let make_spawn e1 e2 = failwith "Unimplemented: make_spawn"
let make_send e1 e2 = failwith "Unimplemented: make_send"
let make_recv e = failwith "Unimplemented: make_recv"
let make_return e = failwith "Unimplemented: make_return"
