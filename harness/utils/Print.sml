
(* Print the string arg if the quietness is smaller than the int arg. *)
val printq: IntInf.int -> string -> unit =
  fn n => if !Config.quiet <= n then (fn s => print (s ^ "\n")) else ignore

(* Format string capabilities *)
fun sprintf str args = Format.format str args
fun printqf n str args = printq n (sprintf str args)
val time = Format.STR o Time.toString
val string = Format.STR
val int = Format.INT
val word8 = Format.WORD8
