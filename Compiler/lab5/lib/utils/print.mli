open Core

val n_str : ?ending:string -> string -> int -> string
val pp_grid : int list -> string list -> string list list -> string
val pp_list : ?btwn:string -> ?around:string*string -> ('a -> string) -> 'a list -> string
val pp_opt : ?default:string -> ('a -> string) -> 'a option -> string
val pp_symbol_hashtbl : ?front:string -> ?btwn:string -> ?back:string -> (Symbol.t, 'a) Hashtbl.t -> ('a -> string) -> string