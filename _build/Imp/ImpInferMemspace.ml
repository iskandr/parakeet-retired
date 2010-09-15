open Base
open Imp 


module MemspaceSemantics = struct 
  module Lattice = struct 
    type dims = int list 
    type rank = int 
    type t = GlobalVec of rank | SharedVec of dims | Scalar | Top | Bottom  
    let top = Top 
    let bottom = Bottom 
    let join v1 v2 = match v1, v2 with 
    | Bottom, _ -> v2 
    | _, Bottom -> v1
    | _ when v1 = v2 -> v1 
    | _ -> Top 
  end
  open Lattice 
  
  let rec eval_exp latticeEnv = function 
  | Var id -> PMap.find id latticeEnv
  | Idx (lhs, rhs) ->
      (match eval_exp latticeEnv lhs with 
        | Scalar -> failwith "attempted to index in scalar"
        | GlobalVec 1 -> Scalar 
        | GlobalVec r -> GlobalVec (r - 1)
        | SharedVec [d] -> Scalar
        | SharedVec (d::ds) -> SharedVec ds
        | Top -> failwith "Expression might have multiple Ptx memory spaces" 
        | _ -> failwith "Insufficient information in abstract interpreter"
       )
  | Select (_, _, tExp, fExp) -> 
      let tSpace = eval_exp latticeEnv tExp in 
      let fSpace = eval_exp latticeEnv fExp in 
      if tSpace <> fSpace 
      then failwith "Branches of select statement must agree on memspace"
      else tSpace 
  | _ -> Scalar

end 

module Analysis = ImpAbstractInterp.Make(MemspaceSemantics)
include MemspaceSemantics.Lattice 

let analyze_function fn =
  let globalAdder map id = 
    let ty = PMap.find id fn.tenv in 
    let memspace = 
      if DynType.is_scalar ty then Scalar
      else if DynType.is_vec ty then GlobalVec (DynType.nest_depth ty)
      else failwith "[infer-memspace] non-vector compound type unsupported"
    in PMap.add id memspace map 
  in   
  let args = Array.append fn.input_ids fn.output_ids in 
  let globalsEnv = Array.fold_left globalAdder PMap.empty args in  
  let sharedAdder id dims map = 
      let ty = PMap.find id fn.tenv in 
      let memspace = 
        if DynType.is_scalar ty then Scalar
        else if DynType.is_vec ty then SharedVec dims
        else failwith "[infer-memspace] non-vector compound type unsupported"
      in 
      PMap.add id memspace map      
  in 
  let fullEnv = PMap.foldi sharedAdder fn.shared globalsEnv in  
  Analysis.run fullEnv fn.body
