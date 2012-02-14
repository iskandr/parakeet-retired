open Base
open Llvm
open Value

module GV = Llvm_executionengine.GenericValue
module LLE = Llvm_executionengine.ExecutionEngine

external create_work_queue : int -> Int64.t = "ocaml_create_work_queue"
external destroy_work_queue : Int64.t -> unit = "ocaml_destroy_work_queue"
external do_work : Int64.t -> LLE.t -> llvalue -> GV.t list list -> unit =
    "ocaml_do_work"
