(* top/main.sml : 1.1-18.1 *)
(* molasses-file29.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
(* Run the appropriate main function, forwarding arguments *)

val () =
  OS.Process.exit
    (case CommandLine.arguments () of
       "gradeTests" :: rest => Grader.gradeTests (String.concatWith " " rest)
     | "gradeCompiler" :: rest =>
         Grader.gradeCompiler (String.concatWith " " rest)
     | "benchCompiler" :: rest =>
         Grader.benchCompiler (String.concatWith " " rest)
     | "verifyCheckpoint" :: rest =>
         Grader.verifyCheckpoint (String.concatWith " " rest)
     | args =>
         ( print ("Unrecognized arguments: " ^ String.concatWith " " args)
         ; OS.Process.failure
         ))
structure InternalMolassesStructure2 = struct
	
end
end
