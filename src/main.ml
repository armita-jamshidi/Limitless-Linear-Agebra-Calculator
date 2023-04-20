(******************************************************************************
   You do not need to modify anything in this file.
 ******************************************************************************)

type error =
  | ParseError of string
  | TypeError of string

let interp_expr typ_context (env : Eval.env) (rml_expr : string) =
  match rml_expr |> Parse.parse_expr with
  | Ok expr_ast -> begin
      match Checker.check_expr typ_context expr_ast with
      | Ok (_, prog_ast) ->
          Ok (Eval.eval_expr env prog_ast |> Eval.string_of_value)
      | Error e -> Error (TypeError (e |> Checker.str_of_error))
    end
  | Error e -> Error (ParseError e)

let interp_parse_result typ_context env parse_result =
  match parse_result with
  | Ok expr_ast -> begin
      match Checker.check_prog typ_context expr_ast with
      | Ok (typ_context', prog_ast) ->
          let env' = Eval.eval_program env prog_ast in
          let new_defs = Eval.take_env (Eval.size_env env' - Eval.size_env env) env' in
          Ok (new_defs |> Eval.string_of_env, env', typ_context')
      | Error e -> Error (TypeError (e |> Checker.str_of_error))
    end
  | Error e -> Error (ParseError e)

(** [interp_file_include fn typ_context env fns_included] evaluates the
    file [fn], checking for include statements and interpreting any
    included files in a typing context [typ_context] under an environment
    [env].

    @param [fns_included]
      is the list of filenames already included. Cyclic dependencies are
      disallowed and will throw.
    @return
      A result monad where a valid result is [(str, env', typ_context')] where
      [str'] is the string representation of the new bindings in the environment
      or error, [env'] is the final environment after evaluation and
      [typ_context'] is the final typing context after evaluation. *)
let rec interp_file_include
    (fn : string)
    (typ_context : Checker.Context.t)
    (env : Eval.env)
    (fns_included : string list) :
    (string * Eval.env * Checker.Context.t, error) result =
  match
    interp_include fn (Filename.dirname fn) typ_context env fns_included
  with
  | Ok (env', typ_context') ->
      Parse.parse_file fn |> interp_parse_result typ_context' env'
  | Error e -> Error e

(** [interp_include fn dir_path typ_context env fns_included] evaluates
    the include statements in the program given by filename [fn] in
    directory (relative to the calling directory) [dir_path] under [env]
    and with typing context [typ_context], where [fns_included] is the
    accumulated list of all the filenames already included.

    @return
      A result monad where a valid result [(env', typ_context')] is the
      new [Eval.env] with all definitions from all of the include
      statements in the program with filename [fn] and the new
      [Checker.Context.t] with all definitions from all of the include
      statements in the program with filename [fn].

      NOTE: The returned [env'] or [typ_context'] do not include
      evaluated definitions in the program represented by [fn] itself
      not in include statements. *)
and interp_include
    (fn : string)
    (dir_path : string)
    (typ_context : Checker.Context.t)
    (env : Eval.env)
    (fns_included : string list) :
    (Eval.env * Checker.Context.t, error) result =
  let include_re = Str.regexp "include[ \t]+\"\\([^ \t\r\n\"]+\\)\"" in
  let between_quotes s =
    let first_quote = String.index s '"' in
    let last_quote = String.index_from s (first_quote + 1) '"' in
    try
      Ok (String.sub s (first_quote + 1) (last_quote - first_quote - 1))
    with Invalid_argument s ->
      Error (ParseError ("Malformed include: " ^ s))
  in
  let rec next_include in_channel =
    try
      let line = input_line in_channel in
      if Str.string_match include_re line 0 then between_quotes line
      else next_include in_channel
    with End_of_file -> raise Not_found
  in
  let fn_in_c = open_in fn in
  let rec loop in_c typ_context env fns_included typ_context_acc env_acc
      =
    try
      match next_include in_c with
      | Error e -> Error e
      | Ok next_fn -> (
          let next_full_fn = Filename.concat dir_path next_fn in
          if List.mem next_full_fn fns_included then
            Error
              (ParseError
                 "Error: Circular dependency in include statements.")
          else
            let fns_included' = next_full_fn :: fns_included in
            match
              interp_file_include next_full_fn typ_context env
                fns_included'
            with
            | Ok (_, env', typ_context') ->
                loop in_c typ_context env fns_included
                  (Checker.Context.prepend typ_context' typ_context_acc)
                  (Eval.prepend_env env' env_acc)
            | Error e -> Error e)
    with Not_found ->
      close_in in_c;
      Ok (env_acc, typ_context_acc)
  in
  loop fn_in_c typ_context env fns_included typ_context env

let interp_file fn =
  match
    interp_file_include fn Checker.Context.empty Eval.initial_env []
  with
  | Ok _ -> ()
  | Error (ParseError err_str) -> print_endline err_str
  | Error (TypeError err_str) -> print_endline err_str

let interp_prog typ_context env rml_prog =
  rml_prog |> Parse.parse_prog |> interp_parse_result typ_context env
