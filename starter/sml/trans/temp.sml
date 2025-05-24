(* L1 Compiler
 * Temporaries
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *)
structure Temp :> TEMP =
struct
  type temp = int

  local
    val counter = ref 1
  in
    (* warning: calling reset() may jeopardize uniqueness of temps! *)
    fun reset () = ( counter := 1 )
    fun new () = !counter before ( counter := !counter + 1 )
  end

  fun name t = "%t" ^ Int.toString t

  fun compare (t1,t2) = Int.compare (t1,t2)
end
