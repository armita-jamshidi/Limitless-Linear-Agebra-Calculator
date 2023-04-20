(******************************************************************************
   You do not need to modify anything in this file.
 ******************************************************************************)

(* Acknowledgement:
   This RML REPL is adapted from the REPL provided for JoCalf for CS3110 
   which is adapted from the sample REPL provided as part of the
   Lambda-Term package (c) 2015 by Martin DeMello, released under BSD3. *)

open Rml
open React
open Lwt
open LTerm_text

exception Quit
exception DefineMalformed

module Interpreter = struct
  type repl_state = {
    command_count : int;
    cur_command : string;
    env : Eval.env;
    context : Checker.Context.t;
  }

  let initial_rstate =
    {
      command_count = 1;
      cur_command = "";
      env = Eval.initial_env;
      context = Checker.Context.empty;
    }

  let eol_regex = Str.regexp {|^.*;;|}
  let matches s r =
    if Str.string_match r s 0 then Some (Str.matched_string s) else None

  let rec eval state s =
    match matches s eol_regex with
    | None ->
        let state' = { state with cur_command = state.cur_command ^ s } in
        (state', "")
    | Some last ->
        let last' = String.sub last 0 ((String.length last) - 2) in
        let cmd = state.cur_command ^ last' in
        let state' = { state with
          command_count = state.command_count + 1 ;
          cur_command = ""
        } in
        if cmd = "#quit" then raise Quit
        else if cmd = "#env" then (state', Eval.string_of_env state.env)
        else
          match Main.interp_prog state.context state.env cmd with
          | Error (Main.ParseError _) -> begin
              let value_result = eval_expr state cmd in
              match value_result with
              | Ok value_str -> (state', value_str)
              | Error (Main.ParseError err_str) -> (state', err_str)
              | Error (TypeError err_str) -> (state', err_str)
            end
          | Error (Main.TypeError err_str) -> (state', err_str)
          | Ok (result_str, env', ctx') ->
              let state' = { state' with env = env'; context = ctx' } in
              (state', result_str)

  and eval_expr state s = Main.interp_expr state.context state.env s
end

let make_prompt _state =
  let prompt = "# " in
  eval [ S prompt ]

let make_output _state out =
  let output = if out = "" then "" else Printf.sprintf "%s\n\n" out in
  eval [ S output ]

class read_line ~term ~history ~state =
  object (self)
    inherit LTerm_read_line.read_line ~history ()
    inherit [Zed_string.t] LTerm_read_line.term term
    initializer self#set_prompt (S.const (make_prompt state))
  end

let rec loop term history state =
  Lwt.catch
    (fun () ->
      let rl =
        new read_line
          ~term
          ~history:(LTerm_history.contents history)
          ~state
      in
      rl#run >|= fun command -> Some command)
    (function Sys.Break -> return None | exn -> Lwt.fail exn)
  >>= function
  | Some command ->
      let command_utf8 = Zed_string.to_utf8 command in
      let state, out = Interpreter.eval state command_utf8 in
      LTerm.fprints term (make_output state out) >>= fun () ->
      LTerm_history.add history command;
      loop term history state
  | None -> loop term history state

let main () =
  LTerm_inputrc.load () >>= fun () ->
  Lwt.catch
    (fun () ->
      let state = Interpreter.initial_rstate in
      Lazy.force LTerm.stdout >>= fun term ->
      loop term (LTerm_history.create []) state)
    (function
      | LTerm_read_line.Interrupt | Quit -> Lwt.return ()
      | exn -> Lwt.fail exn)

let () =
  print_endline "RML Repl\n";
  Lwt_main.run (main ())
