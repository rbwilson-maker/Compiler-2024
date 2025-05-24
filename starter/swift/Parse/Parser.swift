//
//  Parser.swift
//  L1 Compiler
//  Parsing
//  Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//
//  Gluing together the pieces produced by flex and bison
//

import Foundation

extension C0ParseNode {
    var children: AnySequence<C0ParseNode> {
        guard let child = self.firstChild?.pointee else { return AnySequence([]) }
        let result = sequence(first: child) { (current) -> C0ParseNode? in
            current.nextSibling?.pointee
        }
        return AnySequence(result)
    }
}

extension Sequence {
    var first: Self.Iterator.Element? {
        var iter = self.makeIterator()
        return iter.next()
    }
}

class Parser {
    private static var callback: (AST.Program)->() = {_ in}

    class func parse(_ input: Data, length: Int, completion: @escaping (AST.Program)->() = {_ in}) {
        input.withUnsafeBytes { (toScan: UnsafeRawBufferPointer) -> Void in
            let buf = yy_scan_bytes(toScan.load(as: UnsafePointer<Int8>.self), Int32(length));
            self.callback = completion
            parse_callback = {
                if let result = parseResult?.pointee {
                    Parser.callback(Parser.convertProgram(parseTree: result))
                    free_all_strings()
                    node_free(parseResult)
                } else {
                    let msg: String? = parseError == nil ? nil : String(cString: parseError)
                    print("Parse error: \(msg ?? "")")
                    free_all_strings()
                    node_free(parseResult)
                    exit(1)
                }
            }
            yyparse()
            yy_delete_buffer(buf)
        }
    }
    
    private class func convertProgram(parseTree: C0ParseNode) -> AST.Program {
        switch parseTree.token {
        case C0TokenTypeStmts:
            return Array(parseTree.children.map { child in
                self.convertStatement(parseNode: child)
            })
        default:
            fatalError("Malformed program: \(parseTree.token)")
        }
    }
    
    private class func convertStatement(parseNode: C0ParseNode) -> AST.Stm {
        let value: String? = parseNode.value == nil ? nil : String(cString: parseNode.value)
        switch parseNode.token {
        case C0TokenTypeStmtNewVar:
            return .declare(.newVar(String(cString: parseNode.value)))
        case C0TokenTypeStmtInit:
            return .declare(.`init`(value!, self.convertExpression(parseNode: parseNode.children.first!)))
        case C0TokenTypeStmtAssignment:
            let iter = parseNode.children.makeIterator()
            let first = iter.next()
            let second = iter.next()
            let dest = String(cString:first!.value)
            if (value == "=") {
                return .assign(dest, self.convertExpression(parseNode: second!))
            } else {
                return .assign(dest, .opExp(convertAsnop(value: value!), [.var(dest),
                    self.convertExpression(parseNode: second!)]))
            }
        case C0TokenTypeStmtReturn:
            return .return(self.convertExpression(parseNode: parseNode.children.first!))
        default:
            fatalError("Malformed statement: \(parseNode.token)")
        }
    }
    
    private class func convertExpression(parseNode: C0ParseNode) -> AST.Exp {
        let value: String? = parseNode.value == nil ? nil : String(cString: parseNode.value)
        switch parseNode.token {
        case C0TokenTypeSymbol:
            return .var(value!)
        case C0TokenTypeDecConstant:
            if let integer = Int32(value!, radix: 10) {
                return .constExp(integer)
            } else if (value == "2147483648") {
                return .constExp(Int32.min)
            } else {
                compilerError("Invalid integer constant: \(value!)")
            }
        case C0TokenTypeHexConstant:
            if let integer = UInt32(value!.dropFirst(2), radix: 16) {
                return .constExp(Int32(bitPattern: integer))
            } else {
                compilerError("Invalid integer constant: \(value!)")
            }
        case C0TokenTypePlus, C0TokenTypeMinus, C0TokenTypeTimes, C0TokenTypeDividedBy, C0TokenTypeMod:
            return .opExp(self.convertOper(token: parseNode.token), Array(parseNode.children.map { child in
                return self.convertExpression(parseNode: child)
            }))
        case C0TokenTypeNegative:
            return .opExp(.negative, [self.convertExpression(parseNode: parseNode.children.first!)])
        default:
            fatalError("Malformed expression: \(parseNode.token)")
        }
    }
    
    private class func convertOper(token: C0TokenType) -> AST.Oper {
        switch token {
        case C0TokenTypePlus: return .plus
        case C0TokenTypeMinus: return .minus
        case C0TokenTypeTimes: return .times
        case C0TokenTypeDividedBy: return .dividedby
        case C0TokenTypeMod: return .modulo
        case C0TokenTypeNegative: return .negative
        default:
            fatalError("Malformed operator: \(token)")
        }
    }
    
    private class func convertAsnop(value: String) -> AST.Oper {
        switch value {
        case "+=": return .plus
        case "-=": return .minus
        case "*=": return .times
        case "/=": return .dividedby
        case "%=": return .modulo
        default: fatalError("Malformed asnop: \(value)")
        }
    }
}
