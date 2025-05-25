open Core

module type NodeTy =
sig
  type t
  [@@deriving compare, hash, sexp]

  val to_string : t -> string
end

module Make : functor (N : NodeTy) -> 
sig
    type t
    val empty : t
    val get_nodes : t -> N.t list
    val add_edge : t-> N.t -> N.t -> unit

    (* `make_from_adjlist l` takes an adjacency list and transforms it
     * into an undirected graph. The adjacency list need not be well-formed.
     * 
     * duplicate key-value pairs will union all value sets for that key
     * existence of an edge (a, b) will be propagated such that (b, a) exists
     *)
    val make_from_adjlist : (N.t * N.t list) list -> t

    (* `color ~ordering ~inits graph` 
     * returns a mapping from elements to ints representing colors
     * coloring is done according to the provided `ordering`, or randomly
     * otherwise
     * optionally use `inits` to specify initial colors
     *)
    val color : ?ordering : N.t list -> ?inits : (N.t * int) list -> t -> (N.t, int) Hashtbl.t

    val to_string : t -> string
end

val run_test : unit -> unit