(* pp: -parser o pa_macro.cmo *)

open Base
open SSA 

module StmtId = Int

module StmtSet = StmtId.Set
module StmtMap = StmtId.Map

let optimize_fn fnTable fn = fn, false 

