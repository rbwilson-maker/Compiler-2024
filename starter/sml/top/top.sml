(* L1 Compiler
 * Top Level Environment
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 *)

signature TOP =
sig
  (* main function for standalone executable
   * use with SMLofNJ.exportFn("heapfilename", Top.main)
   *)
  val main : string * string list -> OS.Process.status

  (* test "arguments"; is the same as executing a saved
   * heap with arguments on the command line
   *)
  val test : string -> OS.Process.status
end

structure Top :> TOP =
struct
  exception EXIT
  exception EXIT_USAGE

  fun say s = TextIO.output (TextIO.stdErr, s ^ "\n")
  fun newline () = TextIO.output (TextIO.stdErr, "\n")
  fun errfn msg = (say (msg ^ "\n") ; raise EXIT_USAGE)

  datatype opt_lvl = Opt_None
  datatype output = X86_64 | AbstractAssem

  (* Initial defaults; can be changed *)
  val flag_only_typecheck = Flag.flag "only_typecheck"
  val flag_verbose = Flag.flag "verbose"
  val flag_ast = Flag.flag "ast"
  val flag_ir = Flag.flag "ir"
  val flag_assem = Flag.flag "assem"
  val output = ref AbstractAssem
  val opt_lvl = ref Opt_None

  fun reset_flags () =
    ( List.app Flag.unset [flag_verbose, flag_ast,
         flag_ir, flag_assem];
      output := AbstractAssem;
      opt_lvl := Opt_None )

  (* Getopt command line parsing *)
  (* The signature file $/smlnj-lib/Util/getopt-sig.sml exists in the
   * SMLNJ distribution on your system, and includes documentation. *)
  structure G = GetOpt
  val header = "Usage: compile [OPTION...] SOURCEFILE\nwhere OPTION is"
  val options =
    [ { short = "t", long=["typecheck-only"]
      , desc=G.NoArg (fn () => Flag.set flag_only_typecheck)
      , help="stop after typechecking"
      }
    , { short = "v", long=["verbose"]
      , desc=G.NoArg (fn () => Flag.set flag_verbose)
      , help="verbose messages"
      }
    ,
      { short = "", long=["dump-ast"]
      , desc=G.NoArg (fn () => Flag.set flag_ast)
      , help="pretty print the AST"
      }
    , { short = "", long=["dump-ir"]
      , desc=G.NoArg (fn () => Flag.set flag_ir)
      , help="pretty print the IR"
      }
    , { short = "", long=["dump-assem"]
      , desc=G.NoArg (fn () => Flag.set flag_assem)
      , help="pretty print abstract assembly"
      }
    , { short = "e"
      , long=["emit"]
      , desc=G.ReqArg ( fn "abs" => output := AbstractAssem
                         | "x86-64" => output := X86_64
                         | x => errfn ("Error: Unknown --emit arg: "^x)
                      , "{abs, x86-64}"
                      )
      , help="Control type of output"
      }
    , { short = "O"
      , long=["opt"]
      , desc=G.ReqArg ( fn "0" => opt_lvl := Opt_None
                         | "1" => errfn "Error: -O1 unimplemented (l2)"
                         | "2" => errfn "Error: -O2 unimplemented (l5)"
                         | x => errfn ("Error: unknown --opt arg: "^x)
                      , "{0,1,2}"
                      )
      , help="Optimization level"}
    ]

  val usageinfo = G.usageInfo {header = header, options = options}

  fun main (name, args) =
    let
      val _ = Temp.reset () (* reset temp variable counter *)
      val _ = reset_flags () (* return all flags to default value *)

      val _ = if List.length args = 0 then raise EXIT_USAGE
        else ()

      val (opts, files) =
        G.getOpt {argOrder = G.Permute,
            options = options,
            errFn = errfn}
          args

      val source =
        case files of
          [] => errfn "Error: no input file"
        | [filename] => filename
        | _ => errfn "Error: more than one input file"

      val _ = Flag.guard flag_verbose say ("Parsing... " ^ source)
      val ast = Parse.parse source
      val _ = Flag.guard flag_ast
        (fn () => say (Ast.Print.pp_program ast)) ()

      val _ = Flag.guard flag_verbose say "Checking..."
      val _ = TypeChecker.typecheck ast
      val _ = Flag.guard flag_only_typecheck OS.Process.exit OS.Process.success

      val _ = Flag.guard flag_verbose say "Translating..."
      val ir = Trans.translate ast
      val _ = Flag.guard flag_ir (fn () => say (Tree.Print.pp_program ir)) ()

      val _ = Flag.guard flag_verbose say "Codegen..."
      val assem = Codegen.codegen ir
      val _ = Flag.guard flag_assem
        (fn () => List.app (TextIO.print o Assem.format) assem) ()

      in
        case !output of
          AbstractAssem =>
            let
              val pfname = source ^ ".abs"
              val full =
                [Assem.DIRECTIVE (".file\t\"" ^ source ^ "\"")]
                @ [Assem.DIRECTIVE (".function\tmain()")]
                @ assem
                @ [Assem.DIRECTIVE (".ident\t\"15-411 L1 compiler\"")]
              val code = String.concat (List.map (Assem.format) full)
            in
              Flag.guard flag_verbose say
                ("Writing abstract assembly to " ^ pfname ^ " ...");
              SafeIO.withOpenOut pfname (fn pfstream =>
                TextIO.output (pfstream, code));
              OS.Process.success
             end
           | X86_64 =>
              let
                val afname = source ^ ".s"
              in
                say "x86-64 output not implemented";
                OS.Process.failure
              end
      end
      handle ErrorMsg.Error => ( say "Compilation failed" ; OS.Process.failure )
           | EXIT => OS.Process.failure
           | EXIT_USAGE => ( say usageinfo; OS.Process.failure )
           | e => (say ("Unrecognized exception " ^ exnName e); OS.Process.failure)

  fun test s = main ("", String.tokens Char.isSpace s)
end
