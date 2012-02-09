open Llvm

module LLE = Llvm_executionengine.ExecutionEngine
module GV = Llvm_executionengine.GenericValue

type work_item = {
  args : GV.t list;
}

external create_work_queue : int -> Int64.t = "ocaml_create_work_queue"
external destroy_work_queue : Int64.t -> unit = "ocaml_destroy_work_queue"
external do_work : Int64.t -> LLE.t -> llvalue -> work_item list -> unit =
    "ocaml_do_work"
