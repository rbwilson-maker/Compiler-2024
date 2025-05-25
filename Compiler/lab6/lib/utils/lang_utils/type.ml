open Core

module Type = struct
  type t = 
    | Int
    | Bool
    | Ident of Symbol.t
    | Ptr of t
    | Array of t
    | Struct of Symbol.t
    | Arrow of t list * ret_typ
    | Anyptr
  [@@deriving equal, compare, hash, sexp]

  and ret_typ = Typ of t | Void
  [@@deriving equal, compare, hash, sexp]

  let rec pp_typ = function
    | Int -> "int"
    | Bool -> "bool"
    | Ident s -> Symbol.name s;
    | Ptr typ -> sprintf "%s*" (pp_typ typ)
    | Array typ -> sprintf "%s[]" (pp_typ typ)
    | Struct s -> sprintf "struct %s" (Symbol.name s)
    | Arrow (typs, rtyp) -> sprintf "(%s) -> %s" (Print.pp_list pp_typ typs) (pp_ret_typ rtyp)
    | Anyptr -> failwith "Tried to print anyptr type"

  and pp_ret_typ = function
    | Typ typ -> pp_typ typ
    | Void -> "void"
  ;;
end

module Tag = struct
  type 'a t = 'a * (Type.t option)

  exception Split_Naked_Error

  let tag (data : 'a) (typ : Type.t) = (data, Some typ) 
  let tag_naked (data : 'a) = (data, None)
  let data : 'a t -> 'a = fst
  let untag : 'a t -> Type.t = fun t ->
    match snd t with None -> raise Split_Naked_Error
    | Some typ -> typ
  let split : 'a t -> 'a * Type.t = fun (a, t) -> 
    match t with None -> raise Split_Naked_Error
    | Some t -> (a, t)
  let untag_opt : 'a t -> Type.t option = snd
  let map ~(f : 'a -> 'b) : 'a t -> 'b t = Tuple2.map_fst ~f
end

include Type
include Comparable.Make(Type)


