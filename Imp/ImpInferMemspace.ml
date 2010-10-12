open Base
open Imp 


module Analysis = ImpAbstractInterp.Make(MemspaceSemantics)
open MemspaceSemantics.Lattice 

let analyze_function fn =
  PMap.iter (fun id t -> Printf.printf "%d: %s\n" id (DynType.to_str t)) fn.tenv; 
  let globalAdder map id = 
    let ty = PMap.find id fn.tenv in
    let memspace = 
      if DynType.is_scalar ty then Scalar
      else if DynType.is_vec ty then GlobalVec (DynType.nest_depth ty)
      else failwith "[infer-memspace] non-vector compound type unexpected for global"
            
    in PMap.add id memspace map 
  in
  let args = Array.append fn.input_ids fn.output_ids in 
  let globalsEnv = Array.fold_left globalAdder PMap.empty args in
  let sharedAdder id dims map = 
    
      assert (PMap.mem id fn.tenv);
      let ty = PMap.find id fn.tenv in 
      let memspace = 
        if DynType.is_scalar ty then Scalar
        else if DynType.is_vec ty then SharedVec dims
        else failwith "[infer-memspace] non-vector compound type unsupported for shared"
      in 
      PMap.add id memspace map      
  in
  let fullEnv = PMap.foldi sharedAdder fn.shared globalsEnv in

  Analysis.run fullEnv fn.body
