open! Core

module Reg : Graph.NodeTy with type t=string
type reg = Reg.t

type line =
  { uses : reg list
  ; defines : reg list
  ; live_out : reg list
  ; move : bool
  ; line_number : int
  }

type program = line list

val program_of_json : Yojson.Basic.t -> program

type allocation = (reg * reg) option
type allocations = allocation list

val json_of_allocations : allocations -> Yojson.Basic.t
