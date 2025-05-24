//
//  Temp.swift
//  L1 Compiler
//  Temporaries
//  Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
//  Modified: Alex Vaynberg <alv@andrew.cmu.edu>
//  Modified: Frank Pfenning <fp@cs.cmu.edu>
//  Converted to OCaml by Michael Duggan <md5i@cs.cmu.edu>
//  Converted to Swift by Jonathan Burns <jaburns@andrew.cmu.edu>
//

struct Temp : CustomStringConvertible, Comparable, Hashable {
    private(set) static var numTemps: Int = 1
    
    let id: Int
    
    init() {
        id = Temp.numTemps
        Temp.numTemps += 1
    }
    
    static func reset() {
        numTemps = 1
    }
    
    var description: String {
        return "%t\(id)"
    }
    
    func hash(into hasher: inout Hasher) {
        hasher.combine(id)
    }
}

func <(lhs: Temp, rhs: Temp) -> Bool {
    return lhs.id < rhs.id
}

func ==(lhs: Temp, rhs: Temp) -> Bool {
    return lhs.id == rhs.id
}
