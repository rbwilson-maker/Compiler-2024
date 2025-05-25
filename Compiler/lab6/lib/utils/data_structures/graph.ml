(* Graph Library
 * Author: Rachel Wilson 
 *
 * Graph library implemented with Hashtables.
 * It's probably pretty inefficient... but coloring is NP Complete anyway.
 * Coloring takes some additional useful parameters for the register allocator.
 *)
open Core

module type NodeTy =
sig
  type t
  [@@deriving compare, hash, sexp]
  val to_string : t -> string
end

module Make (N : NodeTy) = 
struct
  type elem = N.t
  type t = (elem, elem Hash_set.t) Hashtbl.t
  let empty : t = Hashtbl.create (module N)
  let get_nodes : t -> elem list = Hashtbl.keys 

  let to_string (g : t) = 
    "{\n" 
    ^ (Hashtbl.fold g ~init:"" ~f:(fun ~key ~data out_string -> 
          out_string
        ^ N.to_string key 
        ^ ": "
        ^ Print.pp_list ~around:("[","]") N.to_string (Hash_set.to_list data) 
        ^ "\n")) 
    ^ "}"

  let add_edge (g : t) (a : elem) (b : elem) : unit = 
    (* a -> b *)
    (match Hashtbl.find g a with
    | None -> Hashtbl.set g ~key:a ~data:(
      let y = Hash_set.create (module N) in 
      Hash_set.add y b; y
    )
    | Some s -> Hashtbl.set g ~key:a ~data:(Hash_set.add s b; s)
    ); 
    (* b -> a *)
    (match Hashtbl.find g b with
    | None -> Hashtbl.set g ~key:b ~data:(
      let y = Hash_set.create (module N) in 
      Hash_set.add y a; y
    )
    | Some s -> Hashtbl.set g ~key:b ~data:(Hash_set.add s a; s)
    ); 
  ;;

  let mcs (g : t) : int * N.t list = 
    let module PQ = Pq.Make(N)(Int) in
    let cardinalities = Hashtbl.create (module N) in
    let working_set = PQ.of_list (get_nodes g) 0 in
    let rec iterate ordering = 
      match PQ.deq working_set with
      | None -> ordering
      | Some v -> 
        Hash_set.iter (Hashtbl.find_exn g v) ~f:(fun nbor -> 
          Hashtbl.update cardinalities nbor ~f:(fun x -> 
            match x with None -> 1 | Some x -> x + 1);
          PQ.change working_set nbor (fun x -> Option.map x ~f:(fun x -> x +1))
        );
        iterate (v::ordering)
    in 
    (Hashtbl.fold cardinalities ~init:0 ~f:(fun ~key:_ ~data m -> Int.max data m)
    , List.rev (iterate []))
  ;;

  let make_from_adjlist (interferences : (elem * elem Hash_set.t) list) : t = 
    (* create hash table with possibly multiple values for a key *)
    let h = Hashtbl.of_alist_multi (module N) (interferences) in
    (* union the duplicate sets *)
    let h = Hashtbl.map h 
      ~f:(fun l -> List.fold l ~init:(Hash_set.create (module N)) ~f:Hash_set.union)
    in let h_copy = Hashtbl.create (module N) in
    (* make edges bidirectional *)
    Hashtbl.iteri h ~f:(fun ~key:d ~data:nbors ->
      (* Add d to the neighbor sets of all its neighbors *)
      Hashtbl.update h_copy d ~f:(fun s ->
        match s with
        | None -> Hash_set.copy nbors
        | Some s -> Hash_set.union s nbors
      );
      Hash_set.iter nbors ~f:(fun d' ->
        Hashtbl.update h_copy d' ~f:(fun s ->
          match s with
          | None -> Hash_set.of_list (module N) [d]
          | Some s -> Hash_set.add s d; s
          )
        )
    ); h_copy
  ;;

  let color ?ordering ?inits (graph : t): (elem, int) Hashtbl.t =
    let coloring = Hashtbl.create (module N) in
    (match inits with
     | None -> ()
     | Some inits -> List.iter inits 
     ~f:(fun (key, data) -> Hashtbl.set coloring ~key ~data)
    );
    (* updates the coloring for a given element *)
    let do_color ~key:(node : elem) ~data:nbors () = (
      let nbor_colors = Hash_set.fold nbors ~init:[] ~f:(fun l n ->
        match Hashtbl.find coloring n with
        | None -> l
        | Some c -> c::l)
      in
      (* `lowest_new_color l i` starts at `i` and finds the
       * next lowest color that is not already in `l`
       * Requires a sorted input list `l` *)
      let rec lowest_new_color l i =
        match l with
        | [] -> i
        | c::l' -> ( 
          match Int.compare i c with
          | 0 -> lowest_new_color l' (i+1)
          | x when x < 0 -> i
          | _ -> lowest_new_color l' i
        )
      in
      Hashtbl.update coloring node ~f:(fun v ->
        match v with
        | None -> lowest_new_color (List.sort ~compare:Int.compare nbor_colors) 0
        | Some c -> c)
    ) in
    let do_color_list () (node : elem) = (
      match Hashtbl.find graph node with
      | None -> failwith "While iterating over the ordering, an element did not exist in the graph"
      | Some nbors -> do_color ~key:node ~data:nbors ()
    ) in
    match ordering with 
    | None -> Hashtbl.fold graph ~init:() ~f:do_color; coloring
    | Some l -> List.fold l ~init:() ~f:do_color_list; coloring
  ;;
end