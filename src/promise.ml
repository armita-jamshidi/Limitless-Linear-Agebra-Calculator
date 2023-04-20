module Mwt = struct
  type 'a state =
    | Pending
    | Resolved of 'a
    | Rejected of exn

  type 'a handler = 'a state -> unit
  (** RI: the input may not be [Pending] *)

  type 'a promise = {
    mutable state : 'a state;
    mutable handlers : 'a handler list;
  }
  (** RI: if [state <> Pending] then [handlers = \[\]]. *)

  let enqueue (handler : 'a state -> unit) (promise : 'a promise) : unit
      =
    promise.handlers <- handler :: promise.handlers

  type 'a resolver = 'a promise

  type 'a t = 'a promise
  (** External promise type. *)

  (** [write_once p s] changes the state of [p] to be [s]. If [p] and
      [s] are both pending, that has no effect. Raises: [Invalid_arg] if
      the state of [p] is not pending. *)
  let write_once p s =
    if p.state = Pending then p.state <- s
    else invalid_arg "cannot write twice"

  let make () =
    let p = { state = Pending; handlers = [] } in
    (p, p)

  let return x = { state = Resolved x; handlers = [] }
  let state p = p.state

  (** requires: [st] may not be [Pending] *)
  let resolve_or_reject (r : 'a resolver) (st : 'a state) =
    assert (st <> Pending);
    let handlers = r.handlers in
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers

  let reject r x = resolve_or_reject r (Rejected x)
  let resolve r x = resolve_or_reject r (Resolved x)

  let handler (resolver : 'a resolver) : 'a handler = function
    | Pending -> failwith "handler RI violated"
    | Rejected exc -> reject resolver exc
    | Resolved x -> resolve resolver x

  let handler_of_callback
      (callback : 'a -> 'b promise)
      (resolver : 'b resolver) : 'a handler = function
    | Pending -> failwith "handler RI violated"
    | Rejected exc -> reject resolver exc
    | Resolved x -> (
        let promise = callback x in
        match promise.state with
        | Resolved y -> resolve resolver y
        | Rejected exc -> reject resolver exc
        | Pending -> enqueue (handler resolver) promise)

  let bind (input_promise : 'a promise) (callback : 'a -> 'b promise) :
      'b promise =
    match input_promise.state with
    | Resolved x -> callback x
    | Rejected exc -> { state = Rejected exc; handlers = [] }
    | Pending ->
        let output_promise, output_resolver = make () in
        enqueue
          (handler_of_callback callback output_resolver)
          input_promise;
        output_promise

  type 'msg chan = {
    data : 'msg Queue.t;
    resolvers : 'msg resolver Queue.t;
  }
  (** Type of channels sending and receiving ['msg]. *)

  (** [make_chan ()] is a new channel. *)
  let make_chan () : 'msg chan = failwith "Unimplemented"

  (** [recv chan] is a [msg promise] that resolves to the next message
      on [chan] More specifically, if channel is non-empty, gets the
      message. Otherwise, waits for next message on [chan]. *)
  let recv_chan (chan : 'msg chan) : 'msg promise =
    failwith "Unimplemented"

  (** [send chan msg] is a unit promise that resolves when send is
      complete. More specifically, pushes [msg] into [chan] and returns
      a unit promise when done. *)
  let send_chan (chan : 'msg chan) (v : 'msg) : unit promise =
    failwith "Unimplemented"

  module Infix = struct
    let ( >>= ) = bind
  end
end

module Fwt = struct
  type task_id = int
  type chan_id = int

  let fresh_task_id =
    let counter = ref 1 in
    fun () ->
      counter := !counter + 1;
      !counter - 1

  let fresh_chan_id =
    let counter = ref 1 in
    fun () ->
      counter := !counter + 1;
      !counter - 1

  type 'msg chan = {
    data : 'msg Queue.t;
    id : chan_id;
  }
  (** Type of channels sending and receiving ['msg]. *)

  (** Type of promise producing ['a] *)
  type 'a state =
    | Ready : 'a -> 'a state
    | BndFut : 'a bnd_state -> 'a state
    | SelFut : 'a sel_state -> 'a state
    | JoinFut : ('b, 'c) join_state -> ('b * 'c) state
    | RecvFut : 'a recv_state -> 'a state
    | SendFut : send_state -> unit state

  (** State of bind promise *)
  and 'a bnd_state =
    | BndFst : 'b t * ('b -> 'a t) -> 'a bnd_state
    | BndSnd : 'a t -> 'a bnd_state

  (** State of select promise *)
  and 'a sel_state = SelWait of 'a t * 'a t

  (** State of join promise *)
  and ('b, 'c) join_state =
    | JoinBoth of 'b t * 'c t
    | JoinFst of 'b t * 'c
    | JoinSnd of 'b * 'c t

  (** State of recv promise *)
  and 'a recv_state = RecvWait of 'a chan

  (** State of send promise *)
  and send_state = SendWait : 'msg * 'msg chan -> send_state

  and 'a promise = {
    id : task_id;
    mutable state : 'a state;
  }
  (** Main promise type: an identifier, and a mutable state. *)

  and 'a t = 'a promise
  (** External promise type. *)

  (** Hide the return type of a promise. *)
  type task = MkTask : 'a promise -> task

  type executor = {
    tasks : (task_id, task) Hashtbl.t;
    ready : (task_id * task) Queue.t;
  }
  (** Type of executor.

      [tasks] is a hashtable mapping task id to tasks. [ready] is a
      queue of tasks that are ready for another [poll]. *)

  type reactor = { wakers : (chan_id, task_id) Hashtbl.t }
  (** Type of reactor.

      [wakers] is a hashtable mapping channel id to task ids that are
      waiting on this channel. A single channel can have more than one
      task waiting on it, so the hashtable tracks all waiters.

      In a more full-featured library, the reactor is responsible for
      waking up tasks from other kinds of events, like timers going off
      and disk/network I/O. We have split the executor and reactor types
      so that they can run on different threads, though we will not
      build a multithreaded runtime. *)

  let executor = { tasks = Hashtbl.create 100; ready = Queue.create () }
  let reactor = { wakers = Hashtbl.create 100 }

  (** Put a task into the ready queue. *)
  let wake_task (tid : task_id) : unit =
    Queue.add (tid, Hashtbl.find executor.tasks tid) executor.ready

  (** Poll a promise.

      At a high level, each promise is either ready, or waiting on
      something. Polling a promise asks it to check if whatever it is
      waiting on is ready. If a promise is waiting on a send/receive
      from a channel, we record the record channel and [root] task in
      the reactor. If a promise is waiting on another promise, we poll
      recursively. Polling a promise can update the state of the promise
      if for instance the original child we are waiting on is done, and
      we are now waiting on some other child.
  *)
  let rec poll : task_id -> task -> unit =
    fun _ _ -> failwith "Unimplemented"

  (** Poll all tasks in the ready queue until it is empty.

      This is the main loop of the promises library: it asks each task
      that might be able to make progress to make progress until it is
      blocked. If the ready queue is empty, then no task can make
      progress until some event happens. Since the only events supported
      by the runtime are send and receive on channels---which must come
      from other promises---this means that no task can make progress
      until a new promise is created. *)
  let rec poll_ready () =
    match Queue.take_opt executor.ready with
    | Some (tid, task) ->
        poll tid task;
        poll_ready ()
    | None -> ()

  (** Given a state [state], [next_promise] returns a promise whose
      field [state] is [state] and [id] is the next task id. *)
  let next_promise state =
    let promise = { id = fresh_task_id (); state } in
    Hashtbl.add executor.tasks promise.id (MkTask promise);
    Queue.push (promise.id, MkTask promise) executor.ready;
    poll_ready ();
    promise

  let return (v : 'a) : 'a t = next_promise (Ready v)

  let bind (f : 'a t) (cont : 'a -> 'b t) : 'b t =
    next_promise (BndFut (BndFst (f, cont)))

  let select (f : 'a t) (g : 'a t) : 'a t =
    next_promise (SelFut (SelWait (f, g)))

  let join (f : 'b t) (g : 'c t) : ('b * 'c) t =
    next_promise (JoinFut (JoinBoth (f, g)))

  (** Make a new channel with a fresh channel id. *)
  let make_chan () : 'msg chan =
    { data = Queue.create (); id = fresh_chan_id () }

  (** [recv chan] is a [msg promise] that resolves to the next message
      on [chan] More specifically, if channel is non-empty, get message.
      Otherwise, wait for next message on [chan]. *)
  let recv_chan (chan : 'msg chan) : 'msg t =
    next_promise (RecvFut (RecvWait chan))

  (** [send chan msg] is a unit promise that resolves when send is
      complete. More specifically, if not full, push value, lookup
      waiting task in reactor, and mark task as ready in the executor.
      Otherwise, mark to put value into l channel id. *)
  let send_chan (chan : 'msg chan) (v : 'msg) : unit t =
    next_promise (SendFut (SendWait (v, chan)))

  module Infix = struct
    let ( >>= ) = bind
  end
end
