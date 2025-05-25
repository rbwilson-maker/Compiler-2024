(* Top Level Environment
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Modified: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
 * Modified: Alice Rao <alrao@andrew.cmu.edu>
 * Modified: Nick Roberts <nroberts@alumni.cmu.edu>
 *   - Use Cmdliner instead of Getopt for command-line parsing.
 * Modified: Henry Nelson <hcnelson99@gmail.com>
 *   - Switch from ocamlbuild to dune 2.7
 *   - TODO: Add support for expect tests
 *   - Update to Core v0.14
 * 
 * Modified: Rachel Wilson and Nicole Fang
 *   - way more testing flags
 *)

open Core

(* Command line arguments *)

module Opt_level = struct
  type t = Opt_none | Opt_one 

  let show = function
    | Opt_none -> "O0"
    | Opt_one -> "O1"
  ;;

  let parse = function
    | "0" -> Result.Ok Opt_none
    | "1" -> Result.Ok Opt_one
    | "2" -> Result.Error (`Msg "Error: -O2 unimplemented (lab 5)")
    | arg -> Result.Error (`Msg ("Error: Unknown --opt arg: " ^ arg))
  ;;

  let conv =
    let print ppf opt = Format.fprintf ppf "%s" (show opt) in
    Cmdliner.Arg.conv (parse, print)
  ;;
end

module Emit = struct
  type t =
    | X86_64
    | Abstract_assem
    | None

  let show = function
    | X86_64 -> "x86-64"
    | Abstract_assem -> "abs"
    | None -> "none"
  ;;

  let parse = function
    | "abs" -> Result.Ok Abstract_assem
    | "x86-64" -> Result.Ok X86_64
    | "none" -> Result.Ok None
    | arg -> Result.Error (`Msg ("Unknown emit arg: " ^ arg))
  ;;

  let conv =
    let print ppf emit = Format.fprintf ppf "%s" (show emit) in
    Cmdliner.Arg.conv (parse, print)
  ;;
end

type cmd_line_args =
  { verbose : bool
  ; unsafe : bool
  ; debug : bool
  ; dump_parsing : bool
  ; dump_ast : bool
  ; dump_elab : bool
  ; dump_ir : bool
  ; dump_assem : bool
  ; parse_only : bool
  ; irtree_only : bool
  ; typecheck_only : bool
  ; count_lines : bool
  ; time_compiler : bool
  ; link : string option
  ; exe : bool
  ; emit : Emit.t
  ; opt_level : Opt_level.t
  ; filename : string
  }

(* A term (using the vocabulary of the Cmdliner library) that can be used to
 * parse command-line arguments. *)
let cmd_line_term : cmd_line_args Cmdliner.Term.t =
  let open Cmdliner in
  (* See https://github.com/janestreet/ppx_let *)
  (* This allows a more convenient syntax for using the Cmdliner
   * library: If we use let%map instead of normal "let", and we
   * have a declaration of the form:
   *
   * let%map x = e1 in e2
   *
   * even if e1 is of type 'a Term.t, we can use x as having type 'a
   * in the body of e2.
   *)
  let module Let_syntax = struct
    let return = Term.const
    let map ~f a = Term.(return f $ a)
    let both a b = Term.(const Tuple2.create $ a $ b)
  end
  in
  let flag info = Arg.value (Arg.flag info) in
  let opt conv ~default info = Arg.value (Arg.opt conv default info) in
  let%map verbose =
    let doc = "If present, print verbose debug information." in
    flag (Arg.info [ "v"; "verbose" ] ~doc)
  and unsafe = 
    let doc = "If present, do not perform safety checks" in
    flag (Arg.info [ "u"; "unsafe" ] ~doc)
  and debug = 
    let doc = "If present, print internal data structures." in
    flag (Arg.info [ "d"; "debug" ] ~doc)
  and dump_parsing =
    let doc = "If present, print debug informaton from parsing." in
    flag (Arg.info [ "dump-parsing" ] ~doc)
  and dump_ast =
    let doc = "If present, print the parsed ast." in
    flag (Arg.info [ "dump-ast" ] ~doc)
  and dump_elab = 
    let doc = "If present, print the elaborated ast." in
    flag (Arg.info ["dump-elab"] ~doc)
  and dump_ir =
    let doc = "If present, print the translated ir ast." in
    flag (Arg.info [ "dump-ir" ] ~doc)
  and dump_assem =
    let doc = "If present, print the final assembly." in
    flag (Arg.info [ "dump-assem" ] ~doc)
  and parse_only = 
    let doc = "If present, exit after parsing." in
    flag (Arg.info [ "p"; "parse-only" ] ~doc)
  and irtree_only =
    let doc = "If present, exit after building the IR Tree." in
    flag (Arg.info ["i"; "irtree-only"] ~doc)
  and count_lines = 
    let doc = "If present, report the lines of ex86-64 code generated" in
    flag (Arg.info ["c"; "count-lines"] ~doc)
  and time_compiler =
    let doc = "If present, report the amount of time it took to compile" in
    flag (Arg.info ["s"; "time-compiler"] ~doc)
  and typecheck_only =
    let doc = "If present, exit after typechecking." in
    flag (Arg.info [ "t"; "typecheck-only" ] ~doc)
  and exe = 
    let doc = "If present, does nothing -- because we emit a .s file which can be compiled normally" in
    flag (Arg.info ["exe"] ~doc)
  and link =
    let doc = "Include a header file" in
    opt
      Arg.(some string)
      ~default:None
      (Arg.info [ "l"; "link" ] ~doc ~docv:"HFILE")
  and emit =
    let doc = "[abs|x86-64] The type of assembly $(docv) to emit." in
    opt
      Emit.conv
      ~default:Emit.Abstract_assem
      (Arg.info [ "e"; "emit" ] ~doc ~docv:"TARGET")
  and opt_level =
    let doc = "[0|1|2] The optimization level $(docv)." in
    opt
      Opt_level.conv
      ~default:Opt_level.Opt_one
      (Arg.info [ "O"; "opt" ] ~doc ~docv:"OPT")
  and filename =
    let doc = "The source file $(docv) to compile." in
    Arg.(required (pos 0 (some non_dir_file) None (info [] ~doc ~docv:"FILE")))
  in
  { verbose
  ; unsafe
  ; debug
  ; dump_parsing
  ; dump_ast
  ; dump_elab
  ; dump_ir
  ; dump_assem
  ; parse_only
  ; irtree_only
  ; typecheck_only
  ; count_lines
  ; time_compiler
  ; link
  ; emit
  ; exe
  ; opt_level
  ; filename
  }
;;

let say_if (v : bool) (f : unit -> string) = if v then prerr_endline (f ())
let say_time cond phase t1 t2 =
  say_if cond (fun () ->
    sprintf "\t%s took %s"
      phase
      (Core_kernel_private.Span_float.to_string (Time.abs_diff t1 t2))
  )

(* The main driver for the compiler: runs each phase. *)
let compile (cmd : cmd_line_args) : unit =
  let start_time = Time.now () in
  
  (* Parse *)
  say_if cmd.verbose (fun () -> "Parsing " ^ cmd.filename ^ "...");
  if cmd.dump_parsing then ignore (Parsing.set_trace true : bool);
  let header_ast = Option.map cmd.link ~f:Parse.parse in
  let ast = Parse.parse cmd.filename in
  say_if cmd.dump_ast (fun () -> Print.pp_opt Asts.Marked.Print.pp_program header_ast);
  say_if cmd.dump_ast (fun () -> Asts.Marked.Print.pp_program ast);
  let parse_time = Time.now () in
  say_time cmd.verbose "Parsing" start_time parse_time;
  if cmd.parse_only then exit 0;

  (* Typecheck *)
  say_if cmd.verbose (fun () -> "Typechecking...");
  let tc_data = Typechecker.typecheck ~header:header_ast ast in
  let typecheck_time = Time.now () in
  say_time cmd.verbose "Typechecking" parse_time typecheck_time;
  if cmd.typecheck_only then exit 0;

  (* Elaborate *)
  say_if cmd.verbose (fun () -> "Elaborating...");
  let ast' = Elaborate.elaborate ~tc_data in
  say_if cmd.dump_elab (fun () -> Elab.Print.pp_program ast');
  let elaborating_time = Time.now () in
  say_time cmd.verbose "Elaborating" typecheck_time elaborating_time;
  (* if cmd.parse_only then exit 0; *)

  (* Translate *)
  say_if cmd.verbose (fun () -> "Translating...");
  let ir = Trans.translate ~unsafe:cmd.unsafe ast' in
  let translating_time = Time.now () in
  say_time cmd.verbose "Translating" elaborating_time translating_time;

  (* Tree optimizations *)
  say_if cmd.verbose (fun () -> "Optimizing IR tree...");
  let ir = match cmd.opt_level with
    | Opt_one -> OptimizeTree.run_all cmd.verbose cmd.debug ir
    | _ -> ir
  in
  let trans_opt_time = Time.now () in
  say_time cmd.verbose "Optimizing IR tree" translating_time trans_opt_time;
  say_if cmd.dump_ir (fun () -> Tree.Print.pp_program ir);
  if cmd.irtree_only then exit 0;

  (* Codegen *)
  say_if cmd.verbose (fun () -> "Codegening...");
  let assem =
    Codegen.codegen ~unsafe:cmd.unsafe tc_data.structs ir |> Elabassem.elab_assem
  in
  say_if cmd.dump_assem (fun () -> Assem.Print.pp_program assem);
  let codegen_time = Time.now () in
  say_time cmd.verbose "Codegen" trans_opt_time codegen_time;

  (* Optimize Assem *)
  let assem = 
  (match cmd.opt_level with
  | Opt_one -> 
    say_if cmd.verbose (fun () -> "Optimizing Assem...");
    let assem = OptimizeAssem.run_all cmd.verbose cmd.debug assem in
    assem
  | _ -> assem
  ) in 
  let optimize_time = Time.now () in
  say_time (cmd.verbose && ((fun x -> match x with Opt_level.Opt_none -> false | _ -> true) cmd.opt_level) ) 
  "Optimize" codegen_time optimize_time;
  
  (* Output to a file *)
  match cmd.emit with
  | None -> ()
  | Abstract_assem ->
    let file = cmd.filename ^ ".abs" in
    say_if cmd.verbose (fun () -> sprintf "Writing abstract assem to %s..." file);
    Out_channel.with_file file ~f:(fun out ->
      let output instr = Out_channel.fprintf out "%s\n" (
        Assem.Print.pp_instr instr
      ) in
      output (Assem.Directive (".file\t\"" ^ cmd.filename ^ "\""));
      List.iter assem ~f:(fun f -> output (Assem.Directive (Assem.Print.pp_fun_header f)); List.iter f.body ~f:output);
      output (Assem.Directive ".ident\t\"15-411 L1 reference compiler\""))
  | X86_64 ->
    (* Convert *)
    say_if cmd.verbose (fun () -> "Convert...");
    let output = Convert.convert assem in

    (* Optimize X86 *)
    let output = 
      (match cmd.opt_level with
      | Opt_one -> 
        say_if cmd.verbose (fun () -> "Optimizing X86...");
        let output = Selfmoves.remove_x86 output in output
      | _ -> output
      ) in
    
    let convert_time = Time.now () in
    say_time cmd.verbose "Convert" optimize_time convert_time;
    
    (* Count number of lines *)
    say_if cmd.count_lines (fun () ->
      sprintf
        "%d"
        (List.fold output ~init:0 ~f:(
          fun sum lines -> sum + List.length lines
        ))
    );

    (* Time compiler *)
    say_if cmd.time_compiler (fun () ->
      sprintf
        "%s"
        (Core_kernel_private.Span_float.to_string (Time.abs_diff start_time convert_time))
    );

    let file = cmd.filename ^ ".s" in
    say_if cmd.verbose (fun () -> sprintf "Writing assembly to %s..." file);
    Out_channel.with_file file ~f:(fun out -> 
      let output_instr instr = 
      Out_channel.fprintf out "%s\n" (X86.Print.pp_line instr)
      in List.iter output ~f:(List.iter ~f:output_instr)
    );
    let print_time = Time.now () in
    say_time cmd.verbose "Output" convert_time print_time;
;;

let run (cmd : cmd_line_args) : unit =
  try compile cmd with
  | Error_msg.Error ->
    prerr_endline "Compilation failed.";
    exit 1
;;

(* Compiler entry point
 * Use the cmd_line_term to parse the command line arguments, and pass the
 * parsed args to the run function.
 *)
let main () =
  let open Cmdliner in
  let cmd_line_info = Cmd.info "c0c" ~doc:"Compile a c0c source file." in
  let cli_parse_result : cmd_line_args Cmd.t = Cmd.v cmd_line_info cmd_line_term in
  match Cmd.eval_value cli_parse_result with
  | Ok (`Ok cmd_line) -> run cmd_line
  | Ok `Version -> Stdlib.exit Cmd.Exit.ok
  | Ok `Help -> Stdlib.exit Cmd.Exit.ok
  | Error _ -> Stdlib.exit Cmd.Exit.cli_error
;;
