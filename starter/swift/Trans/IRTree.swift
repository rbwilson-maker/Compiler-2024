//
//  IRTree.swift
//  L1 Compiler
//  IR Trees
//  Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
//  Modified: Alex Vaynberg <alv@andrew.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//

class IRTree {
    typealias Program = [Stm]
    
    indirect enum Stm: CustomStringConvertible {
        case move(Exp, Exp)
        case `return`(Exp)
        
        var description: String {
            switch self {
            case .move(let e1, let e2): return "\(e1) <-- \(e2)"
            case .return(let e): return "return \(e)"
            }
        }
    }
    
    indirect enum Exp: CustomStringConvertible {
        case const(Int32)
        case temp(Temp)
        case binop(Binop, Exp, Exp)
        
        var description: String {
            switch self {
            case .const(let i): return "\(i)"
            case .temp(let t): return "\(t)"
            case .binop(let oper, let e1, let e2): return "(\(e1) \(oper) \(e2))"
            }
        }
    }
    
    enum Binop: CustomStringConvertible {
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
}
