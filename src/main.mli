
(** Functions in main may emit an error type if the function ran into 
    either a parse error, constructed with [ParseError],
    or type error constructed with [TypeError] *)
type error = ParseError of string | TypeError of string 

(** [interp_expr ctx env rml_expr] is either the string representation of the 
    value that rml_expr evaluates to under the environment 
    env and typing context ctx or an error thrown in interpretation. *)
val interp_expr : Checker.Context.t -> Eval.env -> string -> (string, error) result

(** [interp_expr ctx env rml_prog] is either the string representation of the 
    environment that [rml_prog] evaluates to under the typing context [ctx] 
    and enviornment [env] or an error thrown in interpretation. 
    @return (str, env', ctx') where str is the string representation. 
        [env'] is the environment with all the definitions from [rml_prog] 
        [ctx'] is the typing context with all the types of the definitions 
        from [rml_prog] *)
val interp_prog : Checker.Context.t -> Eval.env -> string -> (string * Eval.env * Checker.Context.t, error) result  

(** [interp_file fn] prints the output to standard out or the parse error
    or type checker error to standard out. *)
val interp_file : string -> unit