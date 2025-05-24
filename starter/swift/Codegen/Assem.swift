//
//  Assem.swift
//  L1 Compiler
//  Assembly language
//  Author: Kaustuv Chaudhuri <kaustuv+@andrew.cmu.edu>
//  Modified By: Alex Vaynberg <alv@andrew.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//
//  Currently just a pseudo language with 3-operand
//  instructions and arbitrarily many temps
//
//  We write
//
//  BINOP  operand1 <- operand2,operand3
//  MOV    operand1 <- operand2
//

class Assem {

    typealias Program = [Instr]

    enum Reg: CustomStringConvertible {
        case eax
        
        var description: String {
            switch self {
            case .eax: return "%eax"
            }
        }
    }
    
    enum Operand: CustomStringConvertible {
        case imm(Int32)
        case reg(Reg)
        case temp(Temp)
        
        var description: String {
            switch self {
            case .imm(let val): return "$\(val)"
            case .reg(let r): return r.description
            case .temp(let t): return t.description
            }
        }
    }
    
    enum Operation: CustomStringConvertible {
        case add, sub, mul, div, mod

        var description: String {
            switch self {
            case .add: return "+"
            case .sub: return "-"
            case .mul: return "*"
            case .div: return "/"
            case .mod: return "%"
            }
        }

    }

    enum Instr: CustomStringConvertible {
        case binop(Operation, Operand, Operand, Operand)
        case mov(Operand, Operand)
        case directive(String)
        case comment(String)

        var description: String {
            switch self {
            case .binop(let oper, let d, let s1, let s2):
                return "\t\(d) <-- \(s1) \(oper) \(s2)\n"
            case .mov(let d, let s):
                return "\t\(d) <-- \(s)\n"
            case .directive(let str):
                return "\(str)\n"
            case .comment(let str):
                return "\t/*\(str)*/\n"
            }
        }
    }

}
