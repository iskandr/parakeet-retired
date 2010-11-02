open Base
open SSA

let use_counts = object 
    inherit [(ID.t, int) PMap.t] SSA_Base_Analysis.base_analysis
    method value counts vNode = match vNode.value with  
    | Var id -> 
      if PMap.mem id counts then PMap.add id (PMap.find id counts + 1) counts
      else PMap.add id 1 counts 
    | _ -> counts 
end

let find_use_counts block = 
  SSA_Base_Analysis.eval_block use_counts PMap.empty block