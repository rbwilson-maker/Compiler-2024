(* score/Stats.sig : 1.1-12.1 *)
(* molasses-file17.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
signature STATS =
  sig
    type 'a result = { suite : string
                     , badTests : (string * string) list
                     , goodTests : ((string * 'a * string) * (Summary.cat * Summary.res)) list
                     } list

    val report : 'a result -> unit
  end

end
