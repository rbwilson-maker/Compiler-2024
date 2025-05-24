//
//  Trans.swift
//  L1 Compiler
//  AST -> IR Translator
//  Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
//  Modified by: Alex Vaynberg <alv@andrew.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//

class Trans {
    private static let operations: [AST.Oper : IRTree.Binop] = [
        .plus: .add,
        .minus: .sub,
        .times: .mul,
        .dividedby: .div,
        .modulo: .mod,
        .negative: .sub             //unary to binary!
    ]
    
    private var env: [AST.Ident : Temp] = [:]
    
    public func translate(ast: AST.Program) -> IRTree.Program {
        // ignore code after return
        let truncated = ast.prefix(through: ast.firstIndex { (stm) -> Bool in
            if case .return = stm {
                return true
            }
            return false
        }!)
        return truncated.flatMap(translate)
    }
    
    private func translate(statement: AST.Stm) -> [IRTree.Stm] {
        switch statement {
        case .declare(let d):
            switch d {
            case .newVar(let name):
                env[name] = Temp()
                return []
            case .`init`(let name, let e):
                env[name] = Temp()
                return translate(statement: .assign(name, e))
            }
        case .assign(let name, let e):
            return [.move(.temp(env[name]!), translate(expression: e))]
        case .return(let e):
            return [.return(translate(expression: e))]
        }
    }
    
    private func translate(expression: AST.Exp) -> IRTree.Exp {
        switch expression {
        case .var(let name):
            return .temp(env[name]!)
        case .constExp(let val):
            return .const(val)
        case .opExp(.negative, let exps):
            assert(exps.count == 1)
            return .binop(Trans.operations[.negative]!, .const(0), translate(expression: exps[0]))
        case .opExp(let oper, let exps):
            assert(exps.count == 2)
            return .binop(Trans.operations[oper]!, translate(expression: exps[0]), translate(expression: exps[1]))
        }
    }
}
