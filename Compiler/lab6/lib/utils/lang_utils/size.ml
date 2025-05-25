open Core

module Size = 
struct 
  type t = 
  | Byte (* 1 byte  : 8  bits : Byte             *)
  | Word (* 2 bytes : 16 bits : Word             *)
  | Long (* 4 bytes : 32 bits : Long/double word *)
  | Quad (* 8 bytes : 64 bits : Quad word        *)
  [@@deriving equal, hash, sexp, compare]

  type data_size = 
  | Small of t
  | Struct of Int64.t

  (* maps fields -> (size, offset) and stores total struct size*)
  and struct_fields = (data_size * Int64.t) Symbol.Map.t * Int64.t

  type structs = (Symbol.t, struct_fields) Hashtbl.t

  let format = function
  | Byte -> "b"
  | Word -> "w"
  | Long -> "l"
  | Quad -> "q"

  let format_data = function
  | Small s -> format s
  | Struct total -> Int64.to_string total

  let to_int64 = function
  | Small Byte -> 1L
  | Small Word -> 2L
  | Small Long -> 4L
  | Small Quad -> 8L
  | Struct total -> total

  let to_int32 = function
  | Small Byte -> 1l
  | Small Word -> 2l
  | Small Long -> 4l
  | Small Quad -> 8l
  | Struct total -> Int64.to_int32_exn total

  let to_int : t -> int = function
  | Byte -> 1
  | Word -> 2
  | Long -> 4
  | Quad -> 8

  let to_small = function
  | Small x -> x
  | Struct _ -> failwith "Structs cannot be converted to a small size"
end

include Size
include Comparable.Make(Size)

