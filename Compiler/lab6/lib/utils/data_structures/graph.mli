(* Graph Library
 * Author: Rachel Wilson 
 *
 * Fairly standard Graph Library parameterized
 * over a node type.
 *)
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

    (** [make_from_adjlist l] takes an ill-formed list of pairs of nodes and 
     some of their neighbors and transforms it
     into a well-formed adjacency list for an undirected graph.
      
     Duplicate key-value pairs will union all value sets for that key, and
     existence of an edge (a, b) will be propagated such that (b, a) exists
     *)
    val make_from_adjlist : (N.t * N.t Hash_set.t) list -> t

    (** [mcs graph] performs maximum cardinality search on the graph and
     returns an ordering and the maximum cardinality 
     *)
    val mcs : t -> int * N.t list


    (** [color ~ordering ~inits graph] returns a mapping from elements to ints representing colors.
     
     Coloring is greedy according to the provided `ordering`, or randomly otherwise.
     
     Optionally use `inits` to specify initial colors
     *)
    val color : ?ordering:N.t list -> ?inits:(N.t * int) list -> t -> (N.t, int) Hashtbl.t

    val to_string : t -> string
end