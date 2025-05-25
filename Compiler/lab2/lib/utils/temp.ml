(* L1 Compiler
 * Temporaries
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 *)

open Core

module T =
struct
  type t = I of int | S of string 
  [@@deriving sexp, compare, hash]
end

type t = T.t [@@deriving sexp, compare, hash]

let counter = ref 1
let reset () = counter := 1

let create () =
  let t = T.I !counter in
  incr counter;
  t
;;

let create_with_name (name : string) = 
  T.S name
;;

let name t = match t with | T.I t -> "%t" ^ string_of_int t | S s -> s

include Comparable.Make (T)
