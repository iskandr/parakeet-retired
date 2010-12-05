open Base
open SSA


class use_count_collector initCounts : SSA_Transform.transformation = object 
    inherit SSA_Transform.default_transformation
    val counts : (ID.t, int) Hashtbl.t = initCounts  
    method var id = 
      let oldCount = Hashtbl.find_default counts id 0 in 
      Hashtbl.add counts id (oldCount+1);
      SSA_Transform.NoChange 
    method result = counts 
end

let find_fundef_use_counts fundef = 
  let initCounts = Hashtbl.create 127 in 
  List.iter (fun id -> Hashtbl.add initCounts id 1) fundef.output_ids; 
  let c = new use_count_collector initCounts in 
  let _ = SSA_Transform.transform_fundef c fundef in 
  c#result  
  