//
//  TypeChecker.swift
//  L1 Compiler
//  TypeChecker
//  Author: Alex Vaynberg <alv@andrew.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//
//  Simple typechecker that is based on a unit Symbol.table
//  This is all that is needed since there is only an integer type present
//  Also, since only straightline code is accepted, we hack our way
//  around initialization checks here.
//
//  Modified: Anand Subramanian <asubrama@andrew.cmu.edu> Fall 2010
//  Now distinguishes between declarations and initialization
//  Modified: Maxime Serrano <mserrano@andrew.cmu.edu> Fall 2014
//  Should be more up-to-date with modern spec.
//  Modified: Matt Bryant <mbryant@andrew.cmu.edu> Fall 2015
//  Handles undefined variables in unreachable code, significant simplifications
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//

import Foundation

class TypeChecker {
    
    // true: declared and defined
    // false: declared but not defined
    // nil: not declared
    private var env: [AST.Ident : Bool] = [:]
    private var returnFound = false
    
    public func typecheck(ast: AST.Program) {
        _ = ast.map(typecheck)
        if (!returnFound) {
            typeError("main does not return")
        }
    }
    
    private func typecheck(statement: AST.Stm) {
        switch statement {
        case .declare(let d):
            switch d {
            case .newVar(let name):
                if let _ = env[name] {
                    typeError("Redeclared variable \(name)")
                } else {
                    env[name] = false
                }
            case .`init`(let name, let e):
                typecheck(statement: .declare(.newVar(name)))
                typecheck(statement: .assign(name, e))
            }
        case .assign(let name, let e):
            typecheck(expression: e)
            if let _ = env[name] {
                env[name] = true
            } else {
                typeError("Undeclared variable \(name)")
            }
        case .return(let e):
            typecheck(expression: e)
            //define all variables declared before return
            let vars = env.keys
            for variable in vars {
                env[variable] = true
            }
            returnFound = true
        }
    }
    
    private func typecheck(expression: AST.Exp) {
        switch expression {
        case .var(let name):
            if let defined = env[name] {
                if (!defined) {
                    typeError("Undefined variable \(name)")
                }
            } else {
                typeError("Undeclared variable \(name)")
            }
        case .constExp(_):
            break
        case .opExp(_, let exps):
            /* Note: it is syntactically impossible in this language to
             * apply an operator to an incorrect number of arguments
             * so we only check each of the arguments
             */
            _ = exps.map(typecheck)
        }
    }
    
    private func typeError(_ message: String) -> Never {
        print("Type error: \(message)")
        exit(1)
    }
}
