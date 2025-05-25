(*
 * A funny priority queue.
 *)

open! Core

(**
* Represents a type that can be used as a hashtable key.
*)
(* module type Keyable = Base.Hashtbl.Key.S *)
module type Keyable = sig
  include Base.Hashtbl.Key.S
  val to_string : t -> string
end

(**
  Make a module to interact with priority queues that have
  elements of type E.t and priorities of type P.t
*)
module Make (E : Keyable) (P : Keyable) =
struct
  module BH = Binheap.Make (P) (E)

  exception DuplicateElt
  type t = BH.t


  (** Create an empty priority queue. *)
  let empty : t = BH.of_array ~sorted:[||] 0

  (** Create a priority queue with the elements
    from the list and the same initial priority *)
  let of_list (elts : E.t list) (init_p : P.t) : t =
    let len = (List.length elts) in
    let elts = List.map elts ~f:(fun elt -> (init_p, elt)) in
    BH.of_array len ~sorted:(Array.of_list elts) 

  (** Add an element to the priority queue. *)
  let enq (pq : t) (elt : E.t) (new_p : P.t) : unit =
    if BH.contains_value pq elt
    then raise DuplicateElt
    else BH.enq pq new_p elt

  (** Get the highest priority element. *)
  let deq : t -> E.t option = BH.deq

  (** Add, remove, or change the priority of an element in the queue. *)
  let change : t -> E.t -> (P.t option -> P.t option) -> unit = BH.change

  (** Add or change the priority of an element in the queue. *)
  let update (pq : t) (elt : E.t) f : unit = change pq elt (fun p -> Some (f p))
end
