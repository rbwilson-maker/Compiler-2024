(*
 * A funny priority queue.
 *)

(** Represents a type that can be used as a hashtable key. *)
(* module type Keyable = Base.Hashtbl.Key.S *)
module type Keyable = sig
  include Base.Hashtbl.Key.S
  val to_string : t -> string
end
 
(** Make a module to interact with priority queues that have
  elements of type E.t and priorities of type P.t
 *)
module Make : functor (E : Keyable) (P : Keyable) ->
sig
  exception DuplicateElt
  type t

  (** Create an empty priority queue. *)
  val empty : t

  (** Create a priority queue with the elements
    from the list and the same initial priority *)
  val of_list : E.t list -> P.t -> t


  (** Add an element to the priority queue. *)
  val enq : t -> E.t -> P.t -> unit

  (** Get (and remove) the highest priority element. *)
  val deq : t -> E.t option

  (** Add, remove, or change the priority of an element in the queue. *)
  val change : t -> E.t -> (P.t option -> P.t option) -> unit
  
  (** Add or change the priority of an element in the queue. *)
  val update : t -> E.t -> (P.t option -> P.t) -> unit
end