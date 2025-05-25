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
        ^ List.to_string ~f:N.to_string (Hash_set.to_list data) 
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

  let make_from_adjlist (interferences : (elem * elem list) list) : t = 
    (* turn lists into sets *)
    let infs' = List.map interferences 
    ~f:(fun (x, l) -> (x, Hash_set.of_list (module N) l)) in
    (* create hash table with possible duplicate values for a key *)
    let h = Hashtbl.of_alist_multi (module N) (infs') in
    (* union the duplicate sets *)
    let h = Hashtbl.map h 
      ~f:(fun l -> 
          List.fold l 
          ~init:(Hash_set.create (module N)) 
          ~f:Hash_set.union)
    in let out = Hashtbl.create (module N) in
    (* copy so not modifying while iterating *)
    (* make edges bidirectional *)
    Hashtbl.iteri h ~f:(fun ~key:d ~data:nbors ->
      (* Add d to the neighbor sets of all its neighbors *)
      Hashtbl.update out d ~f:(fun s ->
        match s with
        | None -> Hash_set.copy nbors
        | Some s -> Hash_set.union s nbors
      );
      Hash_set.iter nbors ~f:(fun d' ->
        Hashtbl.update out d' ~f:(fun s ->
          match s with
          | None -> Hash_set.of_list (module N) [d]
          | Some s -> Hash_set.add s d; s
          )
        )
    ); out
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

let run_test () =
  let module Test : NodeTy with type t=int = 
  struct
    type t = int [@@deriving compare, hash, sexp]
    let to_string = Int.to_string
  end
  in let module X = Make(Test) in
  let g = X.make_from_adjlist [
    (1, [2;3;4])
  ; (2, [1;3;4;5])
  ; (3, [1;2;5])
  ; (1, [5])
  ; (4, [1;2])
  ; (5, [1;2;3])
  ] in
  print_endline (X.to_string g);
;;