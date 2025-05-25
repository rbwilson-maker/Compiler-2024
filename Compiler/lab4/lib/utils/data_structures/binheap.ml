(*
Based on the 15-210 description of a binary heap, which is as follows:

A (complete) binary heap is a particular implementation
of a heap that maintains two invariants:
- Shape property: A complete binary tree (all the levels of the tree are
  completely filled except the bottom level, which is filled from the left).
- Heap property.

Because of the shape property, a binary heap can be maintained in a
sequence with the root in position 0 and the following simple functions for
determining the left child, right child and parent of the node at location i:
- left i = 2 * i + 1
- right i = 2 * i + 2
- parent i = ceil(i/2) - 1

If the resulting index is out of range, then there is no left child,
right child, or parent, respectively.

Insertion can be implemented by adding the new key to the end of the sequence,
and then traversing from that leaf to the root swapping with the parent if
less than the parent. Deletion can be implemented by removing the root and
replacing with the last key in the sequence, and then moving down the tree if
either child of the node is less than the key at the node. Binary heaps have
the same asymptotic bounds as balanced binary search trees, but are likely
faster in practice if the maximum size of the priority queue is known ahead
of time. If the maximum size is not known, then some form of dynamically
sized array is needed.
*)

(*
Limitations:
- Cannot contain more elements than it was originally init with (array impl, no UBA)
- Cannot contain not multiple copies of the same value due to hashtable use
*)

open! Core

(** Represents a type that can be used as a hashtable key. *)
(* module type Keyable = Base.Hashtbl.Key.S *)
module type Keyable = sig
  include Base.Hashtbl.Key.S
  val to_string : t -> string
end

(** Make a module to interact with binary heaps that have
  keys of type K.t and values of type V.t
 *)
module Make = functor (K : Keyable) (V : Keyable) ->
struct
  type t = {
    elts : (K.t * V.t) array; (* Todo: replace array with unbounded array *)
    parents : (V.t, [`Left_c of V.t | `Right_c of V.t | `Root]) Hashtbl.t;
    size : int ref; (* Size - 1 *)
  }

  let left_idx : int -> int = fun i -> 2 * i + 1
  let right_idx : int -> int = fun i -> 2 * i + 2
  let parent_idx : int -> int = fun i -> (i + 1)/2 - 1

  let is_left : int -> bool = fun i -> i % 2 = 1

  (* let print_tbl (tbl : t) =
    Hashtbl.iteri tbl.parents ~f:(fun ~key:s ~data:c ->
      let parent = match c with
      | `Left_c p -> "Left_c " ^ V.to_string p
      | `Right_c p -> "Right_c " ^ V.to_string p
      | `Root -> "root"
      in
      print_endline (V.to_string s ^ " :: parent: " ^ parent))
  
  let print_bh ({elts; parents; size} : t) =
    print_tbl {elts; parents; size};
      printf "Size : %s\n" (Int.to_string !size);
      print_endline ("Array:");
      let _a = Array.fold elts ~init:(0,1) ~f:(fun (ct, tgt) (k, v) ->
        let (acc, start_str) =
          if ct = tgt
          then ((1, tgt * 2), "\n")
          else ((ct + 1, tgt), "")
        in
        printf "%s%s" start_str ("(" ^ K.to_string k ^ " " ^ V.to_string v ^ ")");
        acc
      )
      in print_endline ""

  exception SomethingWentWrong
  let check_bh {elts; parents; size} (caller : string) : unit =
    let report s =
      print_endline "======== Begin Report ========";
      print_bh {elts; parents; size};
      printf "\nCalled from fn %s\n" caller;
      print_endline s;
      raise SomethingWentWrong
    in
    let size = !size in
    let visited = Hash_set.create (module V) in
    let rec check_subtree (idx : int) =
      if idx < size then (
        let (k,v) = elts.(idx) in
        (* Check if we have seen this element before *)
        (
          match Hash_set.strict_add visited v with
          | Ok _ -> ()
          | Error _ -> report (sprintf "Already visited %s" (V.to_string v))
        );
        (* Check the childrens' parents and the childrens' relative prio's *)
        (if left_idx idx < size
        then (
          let (prio, value) = elts.(left_idx idx) in
          (* Check parent *)
          (
            match Hashtbl.find parents value with
            | None -> report (sprintf "Left child %s not in Hashtbl" (V.to_string value))
            | Some (`Left_c p) ->
              if not (V.compare v p = 0)
              then report (sprintf "Left child %s incorrect Hashtbl parent entry" (V.to_string value))
              else ()
            | Some _ -> report (sprintf "Left child %s incorrect Hashtbl child type entry, checking idx=%s" (V.to_string value) (Int.to_string idx))
          );
          (* Check prio *)
          if K.compare prio k > 0
          then report (sprintf "Left child %s has higher prio than parent" (V.to_string value))
          else ()
        ));
        (if right_idx idx < size
          then (
            let (prio, value) = elts.(right_idx idx) in
            (* Check parent *)
            (
              match Hashtbl.find parents value with
              | None -> report (sprintf "Right child %s not in Hashtbl" (V.to_string value))
              | Some (`Right_c p) ->
                if not (V.compare v p = 0)
                then report (sprintf "Right child %s incorrect Hashtbl parent entry" (V.to_string value))
                else ()
              | Some _ -> report (sprintf "Right child %s incorrect Hashtbl child type entry, checking idx=%s" (V.to_string value) (Int.to_string idx))
            );
            (* Check prio *)
            if K.compare prio k > 0
            then report (sprintf "Right child %s has higher prio than parent" (V.to_string value))
            else ()
          ));
        (* Now recursively check the children *)
        check_subtree (left_idx idx);
        check_subtree (right_idx idx)
      )
    in
    if size > 0
    then (
      match Hashtbl.find parents (snd elts.(0)) with
      | Some `Root -> check_subtree 0
      | _ -> report "Root is not listed as `Root in Hashtbl"
    ) *)

  let of_array ~(sorted : (K.t * V.t) array) (size : int) : t =
    (* Array.sort (fun (k1,_) (k2,_) -> K.compare k1 k2) elts; *)
    let elts = Array.init size ~f:(fun i -> sorted.(size - i - 1)) in
    let parents = Hashtbl.create (module V) in
    let rec set_parents (idx : int) : unit =
      if right_idx idx < size
      then (
        Hashtbl.set parents ~key:(snd elts.(right_idx idx)) ~data:(`Right_c (snd elts.(idx)));
        Hashtbl.set parents ~key:(snd elts.(left_idx idx)) ~data:(`Left_c (snd elts.(idx)));
        set_parents (right_idx idx);
        set_parents (left_idx idx)
      ) else (
        if left_idx idx < size
        then (
          Hashtbl.set parents ~key:(snd elts.(left_idx idx)) ~data:(`Left_c (snd elts.(idx)));
          set_parents (left_idx idx)
        )
      )
    in
    (if size > 0
    then 
      Hashtbl.set parents ~key:(snd elts.(0)) ~data:`Root;
      set_parents 0);
    let size = ref size in
    { elts; parents; size; }

  (** Swap the two elements such that the higher priority element is the parent
      and return true if a swap occurred and false otherwise. *)
  let swap (bh : t) ~(parent_idx : int) ~(child_idx : int) : bool =
    let (k1, v1) = bh.elts.(parent_idx) in
    let (k2, v2) = bh.elts.(child_idx) in
    if   K.compare k1 k2 < 0
    then (
      bh.elts.(parent_idx) <- (k2, v2);
      bh.elts.(child_idx) <- (k1, v1);
      (* Set the new parent's parent *)
      Hashtbl.set bh.parents
        ~key:v2
        ~data:(Hashtbl.find_exn bh.parents v1);
      (* Set the new parent's children's parents *)
      Hashtbl.set bh.parents
        ~key:(snd bh.elts.(left_idx parent_idx))
        ~data:(`Left_c v2);
      (if right_idx parent_idx < !(bh.size)
      then
        Hashtbl.set bh.parents
          ~key:(snd bh.elts.(right_idx parent_idx))
          ~data:(`Right_c v2));
      (* Set the new child's children's parents *)
      (if left_idx child_idx < !(bh.size)
      then
        Hashtbl.set bh.parents
          ~key:(snd bh.elts.(left_idx child_idx))
          ~data:(`Left_c v1));
      (if right_idx child_idx < !(bh.size)
      then
        Hashtbl.set bh.parents
          ~key:(snd bh.elts.(right_idx child_idx))
          ~data:(`Right_c v1));
      true
    )
    else false

  let keep_swapping_left : t -> int -> unit =
    fun bh ->
      let rec ks idx =
        if 0 <= parent_idx idx
           && swap bh ~child_idx:idx ~parent_idx:(parent_idx idx)
        then ks (parent_idx idx)
      in ks

  let keep_swapping_right : t -> int -> unit =
    fun bh ->
      let size = !(bh.size) in
      let rec ks idx =
        let left_idx = left_idx idx in
        let right_idx = right_idx idx in
        let left_opt = if left_idx < size then Some (bh.elts.(left_idx)) else None in
        let right_opt = if right_idx < size then Some (bh.elts.(right_idx)) else None in
        let child_idx = match (left_opt, right_opt) with
          | (None, None) -> None
          | (Some _, None) -> Some left_idx
          | (None, Some _) -> Some right_idx
          | (Some (kl,_), Some (kr,_)) ->
            if K.compare kl kr > 0
            then Some left_idx
            else Some right_idx
        in
        match child_idx with
        | None -> ()
        | Some child_idx ->
          if swap bh ~parent_idx:idx ~child_idx
          then ks child_idx
      in ks

  let enq (bh : t) (key : K.t) (value : V.t) : unit =
    let new_idx = !(bh.size) in
    bh.elts.(new_idx) <- (key, value);
    bh.size := new_idx + 1;
    if new_idx > 0
    then
      let parent_idx = parent_idx new_idx in
      Hashtbl.set bh.parents
        ~key:value
        ~data:(if is_left new_idx then `Left_c (snd (bh.elts.(parent_idx))) else `Right_c (snd (bh.elts.(parent_idx))));
      keep_swapping_left bh new_idx
    else (Hashtbl.set bh.parents ~key:value ~data:`Root)
    
  let remove (bh : t) (idx : int) : (K.t * V.t) option =
    bh.size := !(bh.size) - 1;
    let (kres,vres) = bh.elts.(idx) in
    let (ktail, vtail) = bh.elts.(!(bh.size)) in
    bh.elts.(idx) <- (ktail, vtail);
    Hashtbl.set bh.parents
      ~key:vtail
      ~data:(Hashtbl.find_exn bh.parents vres);
    (if left_idx idx < !(bh.size)
    then Hashtbl.set bh.parents ~key:(snd bh.elts.(left_idx idx)) ~data:(`Left_c vtail));
    (if right_idx idx < !(bh.size)
    then Hashtbl.set bh.parents ~key:(snd bh.elts.(right_idx idx)) ~data:(`Right_c vtail));
    keep_swapping_left bh idx;
    keep_swapping_right bh idx;
    Hashtbl.remove bh.parents vres;
    Some (kres,vres)

  let deq (bh : t) : V.t option =
    match !(bh.size) with
    | 0 -> None
    | _ -> Option.map (remove bh 0) ~f:snd
  
  let contains_value (bh : t) (v : V.t) : bool =
    match Hashtbl.find bh.parents v with
    | None -> false
    | Some _ -> true

  (** Find an element and return its index. *)
  let find (bh : t) (v : V.t) : int option =
    match Hashtbl.find bh.parents v with
    | None -> None
    | Some v_parent ->
      let rec make_rev_path parent_entry = match parent_entry with
      | `Root -> []
      | `Left_c v ->
        `Left :: (make_rev_path (Hashtbl.find_exn bh.parents v))
      | `Right_c v ->
        `Right :: (make_rev_path (Hashtbl.find_exn bh.parents v))
      in
      Some
      (List.fold_right (make_rev_path v_parent) ~init:0 ~f:(fun turn idx_acc ->
        match turn with
        | `Left -> left_idx idx_acc
        | `Right -> right_idx idx_acc
      ))

  let change (bh : t) (v : V.t) (f : K.t option -> K.t option) : unit =
    match find bh v with
    | None -> (
      match f None with
      | None -> ()
      | Some k ->
        enq bh k v
    )
    | Some idx ->
      (* Remove the elt *)
      let k = Option.map (remove bh idx) ~f:fst in
      (match f k with
      | None -> ()
      | Some k -> enq bh k v)
end