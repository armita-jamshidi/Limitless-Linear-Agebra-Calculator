open Promise.Mwt.Infix
open Ast

exception TypeError of string
exception InexhaustivePatterns
exception HandleNotFound of string

(* TODO: add a custom type for values *)
type value = unit
and env = (string * value) list

(*****************************************************************************
  Below are a few simple helper functions you need to implement. These are used
  in various places throughout the system in working with your value and
  environment types. This is also a good spot to add any additional helper
  functions you might need involving values and environments.
 ******************************************************************************)

(** [function_option] is either a closure, recursive closure, or not a
    closure. These correspond to value type constructors in some way
    depending on how you implement the value type.

    See the formal spec for more details on values. *)
type function_option =
  | Func of pat * expr * env
  | FuncRec of pat * expr * env ref
  | NotAFunction

(** [assert_function v] is the parameter pattern, body expression, and
    closure environment from [v].

    Raises if [v] is not a function or a recursive function. Built-in
    functions will also raise. *)
let assert_function (v : value) : pat * expr * env =
  failwith "Unimplemented"

(** [assert_function_option v] is the function_option with parameter
    pattern, body expression, and either closure environment or closure
    environment ref from [v].

    Similar to assert_function, but will not raise. *)
let assert_function_option (v : value) : function_option =
  failwith "Unimplemented"

(** [make_handle h] is the value representing the handle [h]. *)
let make_handle (h : handle) : value =
  failwith "Unimplemented"

(** [make_closure p e env] is the closure representing the pattern [p],
    expression [e], and closure environment [env]. *)
let make_closure (p : pat) (e : expr) (env : env) : value =
  failwith "Unimplemented"

(** [make_recursive_closure p e env] is the closure representing the
    pattern [p], expression [e], and closure environment reference
    [env]. *)
let make_recursive_closure (p : pat) (e : expr) (env : env ref) : value =
  failwith "unimplemented"

(* TODO: add mappings for built-in functions:
  - ["print"],
  - ["println"],
  - ["int_of_string"],
  - ["string_of_int"] and
  - ["_SELF"] (hint: use [make_handle 0]). *)
let initial_env = []

(*****************************************************************************
  Below are some helper functions we have provided for environments.
 ******************************************************************************)

(** [update env x v] is the environment that maps [x] to [v] and maps
    every other variable [y] to whatever [env] maps [y] to. *)
let update_env env x v = (x, v) :: env

(** [pop_env_opt env] is [Some (var, v, env')] if [var] -> [v] is the
    latest mapping in [env] and [env'] is the remaining [env], excluding
    the mapping [var] -> [v]. [pop_env_opt env] is [None] if [env] is
    empty.

    That is,
    [update_env env x1 v1 |> (fun e -> update_env e x2 v2) |> pop_env_opt]
    must equal [Some (x2, v2, update_env env x1 v1)].

    @return None if the environment is empty. *)
let pop_env_opt (env : env) : (id * value * env) option =
  match env with
  | (x, v) :: env_tail -> Some (x, v, env_tail)
  | [] -> None

(** [rev_env env] is the environment [env] with the order of mappings
    reversed.

    That is,
    [update_env empty x1 v1 |> (fun e -> update_env e x2 v2) |> rev_env |> pop_env_opt]
    must equal [Some (x1, v1, env')], where [env'] equals
    [update_env empty x2 v2]. *)
let rev_env (env : env) : env = List.rev env

(** [find_env env x] is the value associated to [x] in [env]. Raises:
    [Not_found] if [x] is not associated to any value in [env]. *)
let find_env (env : env) (x : string) : value = List.assoc x env

let prepend_env (env1 : env) (env2 : env) : env = env1 @ env2

let rec take_env (n : int) (env : env) : env =
  if n = 0 then [] else
    (List.hd env) :: (take_env (n - 1) (List.tl env))

let size_env (env : env) : int = List.length env

let rec string_of_value (v : value) : string =
  failwith "Unimplemented" 
let string_of_env (env : env) : string =
  env
  |> List.map (fun (x, v) ->
         "val " ^ x ^ " = " ^ string_of_value v ^ "\n")
  |> List.fold_left ( ^ ) ""

(****************************************************************************
   These next few functions are helper functions we have implemented to help you
   implement the concurrency features of RML. You should use (send, recv, spawn, self) 
   in your implementation to ensure it behaves correctly. You do not need to 
   understand how they work, but feel free to look at them if you are interested.
   Under no circumstances should you change any of the following implementations,
   nor should you use any of the helper functions below except (send, recv, spawn, self).
 ******************************************************************************)

(** Tracks whether the main thread has been initialized *)
let main_thread_initialized = ref false

(* We don't expect more than 20 threads, on average. *)

(** [mailboxes] is a map from handles to a [Hashtabl.t] which maps
    senders to values.

    If a thread with handle [h1] wants to send a message to a thread
    with handle [h2]. It must send the message to
    [Hashtable.find mailboxes h2 |> (fun mailbox -> Hashtabl.find mailbox h1)]*)
let mailboxes = Hashtbl.create 20

let fresh_handle =
  let counter = ref 1 in
  fun () ->
    incr counter;
    !counter - 1

(** [init_main_mailbox ()] opens a mailbox on thread_id 0 for the main
    thread if one is not already open.

    DO NOT USE THIS IN YOUR CODE. *)
let init_main_mailbox () =
  if !main_thread_initialized then ()
  else begin
    Hashtbl.create 5 |> Hashtbl.add mailboxes 0;
    main_thread_initialized := true
  end

(** [send v h sender_handle] sends [v] asynchronously to the robot at
    handle [h] with [sender_handle] being the sender robot's handle.

    Requires: [h] is a valid handle obtained from [spawn] or [self]
    Requires: [sender_handle] is a valid handle referring to the calling
    thread's own handle. The environment should keep track of the
    calling thread's own handle. *)
let send (s : value) (h : int) (sender_handle : int) : unit =
  init_main_mailbox ();
  let chan =
    match Hashtbl.find_opt mailboxes h with
    | None -> raise (HandleNotFound (string_of_int h))
    | Some address -> begin
        match Hashtbl.find_opt address sender_handle with
        | Some chan -> chan
        | None ->
            let chan = Promise.Mwt.make_chan () in
            Hashtbl.add address sender_handle chan;
            chan
      end
  in
  Promise.Mwt.send_chan chan s |> ignore

(** [recv h receiver_handle] is a promise representing the next value
    sent from the robot at handle [h]. The promise will resolve at some
    time after such a value is sent from the handle.

    Requires: [h] is a valid handle obtained from [spawn] or [self].
    Requires: [receiver_handle] is a valid handle referring to the
    calling thread's own handle. The environment should keep track of
    the calling thread's own handle. *)
let recv (h : int) (receiver_handle : int) : value Promise.Mwt.t =
  init_main_mailbox ();
  let chan =
    match Hashtbl.find_opt mailboxes receiver_handle with
    | None -> raise (HandleNotFound (string_of_int h))
    | Some address -> begin
        match Hashtbl.find_opt address h with
        | Some chan -> chan
        | None ->
            let chan = Promise.Mwt.make_chan () in
            Hashtbl.add address h chan;
            chan
      end
  in
  Promise.Mwt.recv_chan chan

(** [spawn f a] is an expression and environment representing the result
    of spawning [f] applied to [a].

    DO NOT USE THIS IN YOUR CODE. This helper function is meant to be
    called in spawn only. *)
let spawn_expr (f : value) (arg : value) : expr * env =
  let open Ast_factory in
  let p, b, clenv = assert_function f in
  let app = make_app (make_fun p b) (make_var "_spawn_arg") in
  let env = update_env clenv "_spawn_arg" arg in
  (app, env)

(** [empty_env ()] is a helper function for spawn_env and returns an
    environment with no elements. Initial environment elements are also
    absent from [empty_env ()].

    DO NOT USE THIS IN YOUR CODE. This helper function is meant to be
    called in spawn_env and spawn only.

    Requires: pop_initial is implemented correctly. *)
let empty_env () =
  let rec pop_initial env =
    match pop_env_opt env with
    | Some (_, _, env_tail) -> pop_initial env_tail
    | None -> env
  in
  pop_initial initial_env

(** [backpatch_new_env env new_env target_var acc_env] correctly updates
    the [env ref] new_env to have value env where every value which is
    bound to the [id] [target_var] and where the value is a recursive
    closure will have its closure environment changed to new_env,
    correctly backpatching the new_env. This is meant as a helper
    function for [spawn_env]

    DO NOT USE THIS IN YOUR CODE. This helper function is meant to be
    called in spawn_env only. Typical use case is:
    [backpatch_new_env !closure_env closure_env closure_id (empty_env ())] *)
let rec backpatch_new_env
    (env : env)
    (new_env : env ref)
    (target_var : string)
    (acc_env : env) : unit =
  match pop_env_opt env with
  | Some (var, v, env_tail) when var = target_var -> begin
      match assert_function_option v with
      | Func _ ->
          let acc' = update_env acc_env var v in
          backpatch_new_env env_tail new_env target_var acc'
      | FuncRec (p, e, _) ->
          let closure_env' = new_env in
          let new_closure = make_recursive_closure p e closure_env' in
          let acc' = update_env acc_env var new_closure in
          backpatch_new_env env_tail new_env target_var acc'
      | NotAFunction ->
          let acc' = update_env acc_env var v in
          backpatch_new_env env_tail new_env target_var acc'
    end
  | Some (var, v, env_tail) ->
      let acc' = update_env acc_env var v in
      backpatch_new_env env_tail new_env target_var acc'
  | None -> new_env := rev_env acc_env

(** [spawn_env env handle_value acc excluded_vars] is the environment
    [env'] which is the environment [env] with all mappings from key
    ["_SELF"] updated to handle_value, with keys in [excluded_vars]
    ignored for the recursive call.

    DO NOT USE THIS IN YOUR CODE. This helper function is meant to be
    called in spawn only. Typical usage is
    [spawn_env env handle_value (empty_env ()) \[\]]*)
let rec spawn_env
    (env : env)
    (handle_value : int)
    (acc : env)
    (excluded_vars : id list) : env =
  match pop_env_opt env with
  | Some (var, v, env_tail) -> (
      if List.mem var excluded_vars then
        let acc' = update_env acc var v in
        spawn_env env_tail handle_value acc' excluded_vars
      else
        match assert_function_option v with
        | Func (p, e, closure_env) ->
            let closure_env' =
              spawn_env closure_env handle_value (empty_env ())
                excluded_vars
            in
            let new_closure =
              make_closure p e
                (update_env closure_env' "_SELF"
                   (make_handle handle_value))
            in
            let acc' = update_env acc var new_closure in
            spawn_env env_tail handle_value acc' excluded_vars
        | FuncRec (p, e, closure_env) ->
            let closure_env' =
              spawn_env !closure_env handle_value (empty_env ())
                (var :: excluded_vars)
            in
            let new_env_ref =
              ref
                (update_env closure_env' "_SELF"
                   (make_handle handle_value))
            in
            backpatch_new_env !new_env_ref new_env_ref var
              (empty_env ());
            let new_closure = make_recursive_closure p e new_env_ref in
            let acc' = update_env acc var new_closure in
            spawn_env env_tail handle_value acc' excluded_vars
        | NotAFunction ->
            let acc' = update_env acc var v in
            if var <> "_SELF" then
              spawn_env env_tail handle_value acc' excluded_vars
            else spawn_env env_tail handle_value acc excluded_vars)
  | None ->
      let env' =
        update_env (rev_env acc) "_SELF" (make_handle handle_value)
      in
      env'

(** [spawn eval f a] is the handle [h] of a new robot running the
    function [f] with argument [a]. The argument [eval] is used to
    evaluate this function application.

    This call is blocking, guaranteeing that [send] and [recv] calls to
    [h] will be valid after it returns.

    Requires: [eval] is [eval_expr]. *)
let spawn (eval : env -> expr -> value) (f : value) (a : value) : handle
    =
  let open Promise.Mwt.Infix in
  let thread_id = fresh_handle () in
  Hashtbl.create 5 |> Hashtbl.add mailboxes thread_id;
  let app, env = spawn_expr f a in
  let env' = spawn_env env thread_id (empty_env ()) [] in
  let _ = eval env' app in
  thread_id

(** [self env] uses the environment [env] to produce the [self] handle
    value. *)
let self (env : env) : value = find_env env "_SELF"

(***************************************************************************
   The rest of the functions in this file compose the main part of the RML
   interpreter. It is your task to implement these functions. Good luck!
 *****************************************************************************)

let rec bind_pattern (p : pat) (v : value) : env option =
  failwith "Unimplemented"

let rec eval_expr (env : env) (expr : expr) : value =
  failwith "Unimplemented"

let eval_defn (env : env) (defn : defn) : env =
  failwith "Unimplemented"

let eval_program (env : env) (prog : prog) : env =
  failwith "Unimplemented"
