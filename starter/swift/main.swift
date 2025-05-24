//
//  main.swift
//  L1 Compiler
//  Top Level Environment
//
//  Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
//  Modified: Alex Vaynberg <alv@andrew.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//

import Foundation

enum Output {
    case abstractAssembly, x86_64
}

enum OptLevel {
    case None
}

class Flags {
    static var verbose: Bool = false
    static var dump_parsing: Bool = false
    static var dump_ast: Bool = false
    static var dump_ir: Bool = false
    static var dump_assem: Bool = false
    static var typecheck_only: Bool = false
    static var output: Output = .abstractAssembly
    static var opt_lvl: OptLevel = .None
    static var filename: String = ""
}

func compilerError(_ message: String) -> Never {
    print(message)
    exit(1)
}

var skip = false;
for (index,arg) in CommandLine.arguments.enumerated() {
    if (index == 0 || skip) {
        skip = false
        continue
    }
    switch arg {
    case "-v", "--verbose": Flags.verbose = true
    case "--dump-parsing": Flags.dump_parsing = true
    case "--dump-ast": Flags.dump_ast = true
    case "--dump-ir": Flags.dump_ir = true
    case "--dump-assem": Flags.dump_assem = true
    case "-t", "--typecheck-only": Flags.typecheck_only = true
    case "-e", "--emit":
        if (index + 1 >= CommandLine.arguments.count) {
            compilerError("missing argument to \(arg)")
        }
        switch CommandLine.arguments[index+1] {
        case "x86-64": Flags.output = .x86_64
        case "abs": Flags.output = .abstractAssembly
        case let x: compilerError("Unknown output type \(x)")
        }
        skip = true
    case _ where arg.hasPrefix("-e"):
        switch arg.dropFirst(2) {
        case "x86-64": Flags.output = .x86_64
        case "abs": Flags.output = .abstractAssembly
        case let x: compilerError("Unknown output type \(x)")
        }
    case "-O", "--opt":
        if (index + 1 >= CommandLine.arguments.count) {
            compilerError("missing argument to \(arg)")
        }
        switch CommandLine.arguments[index+1] {
        case "0": Flags.opt_lvl = .None
        case "1": compilerError("-O1 unimplemented (lab 2)")
        case "2": compilerError("-O2 unimplemented (lab 5)")
        case let x: compilerError("Unknown --opt arg: \(x)")
        }
        skip = true
    case _ where arg.hasPrefix("-O"):
        switch arg.dropFirst(2) {
        case "0": Flags.opt_lvl = .None
        case "1": compilerError("-O1 unimplemented (lab 2)")
        case "2": compilerError("-O2 unimplemented (lab 5)")
        case let x: compilerError("Unknown --opt arg: \(x)")
        }
    default:
        if (Flags.filename == "") {
            Flags.filename = arg
        } else {
            compilerError("Only one input file is allowed.")
        }
    }
}

if (Flags.filename == "") {
    compilerError("No input file provided.")
}
if (Flags.verbose) {
    print("Reading file...")
}
guard let (code, codeLength) = try? readDataFromFile(path: Flags.filename) else {
    compilerError("The file '\(Flags.filename)' does not exist.")
}

if (Flags.verbose) {
    print("Parsing...")
}
Parser.parse(code, length: codeLength) { ast in
    if (Flags.dump_ast) {
        print(ast.map({"\($0)"}).joined(separator: "\n"))
        print("")
    }
    
    if (Flags.verbose) {
        print("Checking...")
    }
    TypeChecker().typecheck(ast: ast)
    if (Flags.typecheck_only) {
        exit(0)
    }
    
    if (Flags.verbose) {
        print("Translating...")
    }
    let tree = Trans().translate(ast: ast)
    if (Flags.dump_ir) {
        print(tree.map({"\($0)"}).joined(separator: "\n"))
        print("")
    }
    
    if (Flags.verbose) {
        print("Codegen...")
    }
    let assem = CodeGen.codegen(program: tree)
    if (Flags.dump_assem) {
        print(assem.map({"\($0)"}).joined())
        print("")
    }
    
    switch Flags.output {
    case .abstractAssembly:
        var full = [
            .directive(".file\t\"\(Flags.filename)\""),
            .directive(".function\tmain()")
        ] + assem
        full.append(.directive(".ident\t\"15-411 L1 reference compiler\""))
        let outfile = "\(Flags.filename).abs"
        if (Flags.verbose) {
            print("Writing abstract assembly to \(outfile) ...")
        }
        let output = full.reduce("", { (accum, instr) -> String in
            accum.appending(instr.description)
        })
        try! output.write(toFile: outfile, atomically: true, encoding: String.Encoding.utf8)
    case .x86_64:
        compilerError("x86-64 output is not implemented yet")
    }
}
