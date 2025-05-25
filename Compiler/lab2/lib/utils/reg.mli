type t = RAX | RBX | RCX | RDX | RSI | RDI | RBP | RSP
         | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
[@@deriving compare, equal, hash, sexp]

type size =
    | Byte (* 1 byte  : 8  bits : Byte             *)
    | Word (* 2 bytes : 16 bits : Word             *)
    | Long (* 4 bytes : 32 bits : Long/double word *)
    | Quad (* 8 bytes : 64 bits : Quad word        *)

(* Formats register names according to the required size 
   Uses %rXb not %rXl for byte size of registers R8-R15 *)
val format : t -> size -> string