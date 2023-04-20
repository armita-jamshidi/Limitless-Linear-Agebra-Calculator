module Fwt : sig
  type 'a t

  type 'msg chan

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val select : 'a t -> 'a t -> 'a t

  val join : 'a t -> 'b t -> ('a * 'b) t

  val make_chan : unit -> 'msg chan

  val recv_chan : 'msg chan -> 'msg t

  val send_chan : 'msg chan -> 'msg -> unit t

  (** Convenience module for providing infix syntax. *)
  module Infix : sig
    (** Infix operator for [bind]. *)
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t 
  end
end

module Mwt : sig
  type 'a t

  type 'msg chan

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val make_chan : unit -> 'msg chan

  val recv_chan : 'msg chan -> 'msg t

  val send_chan : 'msg chan -> 'msg -> unit t

  (** Convenience module for providing infix syntax. *)
  module Infix : sig
    (** Infix operator for [bind]. *)
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t 
  end
end
