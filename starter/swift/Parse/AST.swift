//
//  AST.swift
//  L1 Compiler
//  Abstract Syntax Trees
//  Author: Alex Vaynberg
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//
//  Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//
//  Forward compatible fragment of C0
//

class AST {
    typealias Program = [Stm]
    typealias Ident = String

    enum Oper: CustomStringConvertible {
        case plus
        case minus
        case times
        case dividedby
        case modulo
        case negative                     // unary minus
        
        var description: String {
            switch self {
            case .plus: return "+"
            case .minus: return "-"
            case .times: return "*"
            case .dividedby: return "/"
            case .modulo: return "%"
            case .negative: return "-"
            }
        }
    }

    enum Exp: CustomStringConvertible {
        case `var`(Ident)
        case constExp(Int32)
        case opExp(Oper, [Exp])
        
        var description: String {
            switch self {
            case .var(let s): return s
            case .constExp(let i): return String(i)
            case .opExp(let oper, let exps):
                return "\(oper)(\(exps.map({$0.description}).joined(separator: ", ")))"
            }
        }
    }

    enum Stm: CustomStringConvertible {
        case declare(Decl)
        case assign(Ident, Exp)
        case `return`(Exp)
        
        var description: String {
            switch self {
            case .declare(let d): return d.description
            case .assign(let id, let e): return "\(id) = \(e)"
            case .return(let e): return "return \(e)"
            }
        }
    }

    enum Decl: CustomStringConvertible {
        case newVar(Ident)
        case `init`(Ident, Exp)
        var description: String {
            switch self {
            case .newVar(let id): return "int \(id)"
            case .`init`(let id, let e): return "int \(id) = \(e)"
            }
        }
    }
}
