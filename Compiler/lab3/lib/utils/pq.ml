(*
 * A funny priority queue.
 *)

open! Core

(**
 * Represents a type that can be used as a hashtable key.
 *)
module type Keyable = Base.Hashtbl.Key.S
 
(**
  Make a module to interact with priority queues that have
  elements of type E.t and priorities of type P.t
*)
module Make (E : Keyable) (P : Keyable) =
struct
  exception DuplicateElt
  type t = ((E.t * P.t) list) ref

  (** Create an empty priority queue. *)
  let empty : t = ref []

  (** Create a priority queue with the elements
    from the list and the same initial priority *)
  let of_list elts init_p : t = ref (List.map elts ~f:(fun elt -> (elt, init_p)))

  (** Add an element to the priority queue. *)
  let enq pq_ref elt new_p : unit =
    let pq = !pq_ref in
    let _check_dup =
      match List.find pq ~f:(fun (e,_p) -> E.compare e elt = 0) with
      | None -> ()
      | Some _ -> raise DuplicateElt
    in
    let hd, tl = List.split_while pq ~f:(fun (_,p) -> P.compare new_p p < 0) in
    pq_ref := hd @ ((elt, new_p) :: tl)

  (** Get the highest priority element. *)
  let deq pq_ref : E.t option = let pq = !pq_ref in
    match pq with
    | [] -> None
    | (e,_) :: pq -> pq_ref := pq; Some e

  (** Add, remove, or change the priority of an element in the queue. *)
  let change pq_ref elt (f : (P.t option -> P.t option)) : unit =
    let pq = !pq_ref in
    let hd, tl = List.split_while pq ~f:(fun (e,_) -> not (E.compare e elt = 0)) in
    let p, tl = match tl with
      | [] -> None, tl
      | (_, p) :: tl -> Some p, tl
    in
    pq_ref := hd @ tl;
    match f p with
    | None -> ()
    | Some prio -> enq pq_ref elt prio

  (** Add or change the priority of an element in the queue. *)
  let update pq_ref elt f : unit = change pq_ref elt (fun p -> Some (f p))
end
