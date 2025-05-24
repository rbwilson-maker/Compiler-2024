//
//  FileRead.swift
//  L1 Compiler
//  Adapted from https://gist.github.com/erica/4d31fed94f3668342623 by Jonathan Burns on 12/10/16.
//

import Foundation

struct CouldNotOpenFile: Error {
    let reason: Int32
}

func readDataFromFile(path: String) throws -> (Data, Int) {
    let fp = fopen(path, "r"); defer {fclose(fp)}
    if (fp == nil) {
        throw CouldNotOpenFile(reason: errno)
    }
    var outputData = Data()
    var length = 0
    let chunkSize = 1024
    let buffer: UnsafeMutablePointer<UInt8> = UnsafeMutablePointer.allocate(capacity: chunkSize);
    defer {buffer.deallocate()}
    repeat {
        let count: Int = fread(buffer, 1, chunkSize-1, fp)
        guard ferror(fp) == 0 else {break}
        if count > 0 {
            let newData = Data(buffer: UnsafeBufferPointer<UInt8>(start: buffer, count: count))
            outputData.append(newData)
            length += count
        }
    } while feof(fp) == 0
    return (outputData, length)
}
