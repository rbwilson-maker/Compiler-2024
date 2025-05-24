//
//  CodeGen.swift
//  L1 Compiler
//  Assembly Code Generator for FAKE assembly
//  Author: Alex Vaynberg <alv@andrew.cmu.edu>
//  Based on code by: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//
//  Implements a "convenient munch" algorithm
//

class CodeGen {
    private static let operations: [IRTree.Binop: Assem.Operation] = [
        .add: .add,
        .sub: .sub,
        .mul: .mul,
        .div: .div,
        .mod: .mod
    ]
    
    public class func codegen(program: IRTree.Program) -> Assem.Program {
        return program.flatMap(codegen)
    }
    
    private class func codegen(statement: IRTree.Stm) -> [Assem.Instr] {
        switch statement {
        case .move(.temp(let d), let s):
            return codegen(expression: s, destination: .temp(d))
        case .return(let e):
            return codegen(expression: e, destination: .reg(.eax))
        default:
            fatalError()
        }
    }
    
    private class func codegen(expression: IRTree.Exp, destination: Assem.Operand) -> [Assem.Instr] {
        switch expression {
        case .const(let val):
            return [.mov(destination, .imm(val))]
        case .temp(let t):
            return [.mov(destination, .temp(t))]
        case .binop(let oper, let e1, let e2):
            let t1 = Temp()
            let t2 = Temp()
            var result = codegen(expression: e1, destination: .temp(t1))
            result.append(contentsOf: codegen(expression: e2, destination: .temp(t2)))
            result.append(.binop(CodeGen.operations[oper]!, destination, .temp(t1), .temp(t2)))
            return result
        }
    }
    
}
