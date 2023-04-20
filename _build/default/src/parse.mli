(** [parse_expr expr_str] is the [Types.expr] value representing the expression 
    represented by the string [expr_str], or a string representation of the 
    appropriate syntax error message if the parser fails on [expr_str]. *)
val parse_expr : string -> (Types.expr, string) result 

(** [parse_prog prog_str] is the [Types.prog] value representing the program
    represented by the string [prog_str], or a string reprsentation of the 
    appropriate syntax error message if the parser fails on [prog_str]. *)
val parse_prog : string -> (Types.prog, string) result

(** [parse_file fn] is the [Types.prog] value representing the program in the 
    file [fn], or a string representation of the appropriate syntax error
    message if the parser fails on the program in [fn]. [fn] is expected to be 
    a relative path to a file. *)
val parse_file : string -> (Types.prog, string) result

