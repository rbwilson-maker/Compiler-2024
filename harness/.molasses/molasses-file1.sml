(* utils/Maps.sml : 1.1-29.1 *)
(* molasses-file1.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
in
structure StringSet =
  RedBlackSetFn
    (struct
       type ord_key = string
       val compare = String.compare
     end)

structure StringMap =
  RedBlackMapFn
    (struct
       type ord_key = string
       val compare = String.compare
     end)

(* Mapping from key to list of values. *)
structure MultiDict =
  struct
    type ('a, 'b) dict = ('a * 'b list) list
    fun add (dict : (''a, 'b) dict) (k : ''a) (v : 'b) : (''a, 'b) dict =
      case dict of
        [] => [(k, [v])]
      | ((k', vs) :: dict) =>
          if k = k' then
            (k', v :: vs) :: dict
          else
            (k', vs) :: add dict k v
  end

end
