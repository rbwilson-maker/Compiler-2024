(* top/Grader.sig : 1.1-15.1 *)
(* molasses-file27.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
(* The string argument to each of these are the command-line arguments
 * to the script. These arguments are processed by the Args structure
 * and used to initialize the Config structure.
 *)
signature GRADER =
  sig
    val verifyCheckpoint : string -> OS.Process.status
    val generateCheckpointInput : string -> OS.Process.status
    val gradeTests : string -> OS.Process.status
    val benchCompiler : string -> OS.Process.status
    val gradeCompiler : string -> OS.Process.status
    val selfTestCompiler : string -> OS.Process.status
  end

end
