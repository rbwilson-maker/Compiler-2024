(* L1 Compiler
 * Temporaries
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *)
signature TEMP =
sig
  type temp

  val reset : unit -> unit	(* resets temp numbering *)
  val new : unit -> temp	(* returns a unique new temp *)
  val name : temp -> string	(* returns the name of a temp *)
  val compare : temp * temp -> order (* comparison function *)
end
