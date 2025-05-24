signature COMPILER =
sig
  datatype lib =
      NOLIB of string
    | DEFAULT of string         (* DEFAULT "l3" versus DEFAULT "l4" *)
    | HEADER of string * string (* dir and base (without h0) *)
                                (* Example: HEADER ("../runtime", "15411-l3") *)

  (* Runs a C0 compiler with cc0-style options. *)
  val runCC0 :
    string * lib * string list
      -> Outcome.t
      -> (Outcome.t * string option)

  (* Runs a C0 compiler with c0c-style options. *)
  type args = string * lib * string list list (* C0C Configuration options *)
  val generateFile : string -> string -> string list -> Outcome.t
  val generateL2RefOutputs : string -> string -> Outcome.t
  val runL1Verifier : string -> IntInf.int -> Outcome.t
  val runL2Verifier : string -> string -> string list -> Outcome.t
  val runC0C : args -> Outcome.t -> (Outcome.t list * string option)

  (* benchmark (filename, optimize, safe) *)
  val benchmark : (string * int * bool) -> Outcome.bench_result

  (* All runtime files to compile with gcc. *)
  val toPrecompile : { inFile : string, outFile : string } list
end
