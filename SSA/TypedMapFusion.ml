
let fusion_valid predTypes succTypes =
  let depths1 = List.map DynType.nest_depth predTypes in
  let depths2 = List.map DynType.nest_depth succTypes in
  assert (List.length depths1 > 0);
  assert (List.length depths2 > 0);  
  let max1 = List.fold_left max (List.hd depths1) depths1 in 
  let max2 = List.fold_left max (List.hd depths2) depths2 in
  (max1 = max2) 
  
  (** could also try to fuse maps which simply share an input, but let's try
      just flow of predecessor to successors for now 
   *) 
  (*
    
   &&    
  let combined1 = List.combine predVals depths1 in 
  let combined2 = List.combine succVals depths2 in
  (* filter out inputs which will be expanded, keeping only arrays of 
     maximal rank 
  *)  
  let maxvals1 = 
    List.fold_left (fun acc (v,d) -> if d = max1 then v::acc else acc) combined1
  in 
  let maxvals2 = 
    List.fold_left (fun acc (v,d) -> if d = max2 then v::acc else acc) combined2   
  in 
  (* either the two maps shared an input of maximal shape, or
     the output of the first is an input to the second 
  *)  
  let set1 = PSet.of_list maxval1 in 
  let set2 = PSet.of_list maxval2 in 
  PSet.compare_sets set1 set2 <> PSet.Disjoint 
  *)

let rec find_fusion_candidate tenv defMap succTypes = function 
  | (Var id)::rest -> 
    (match PMap.find id defMap with
     | ArrayOp(Prim.Map, [fn], vals) -> 
        let types = List.map (TypedCore.type_of_value tenv) vals in 
        if fusion_valid types succArgTypes  then Some (id, fn, vals)
        else find_fusion_candidate tenv defMap succArgTypes rest 
     | _ -> find_fusion_candidate tenv defMap succArgTypes rest 
     )
  | []-> None
  | _::rest -> find_fusion_candidate tenv defMap succArgTypes rest 
 
let rec eval_stmt tenv defMap = function 
  | Set(ids, ArrayOp(Prim.Map, [fn], vals)) ->
    let argTypes = List.map (TypedCore.type_of_value tenv) vs in   
    (match find_fusion_candidate with 
     | Some (predFn, predVals) ->
               
    
    

let rec eval_exp = function 
  
  | App (lhs, args) ->
      let (set1, map1) = eval_value useMap lhs in  
      let (set2, map2) = eval_value_list map1 args in
      PSet.union set1 set2, map2 
  | Cast _ 
  | ScalarOp _
  | ArrayOp _ -> failwith "not yet implemented"
  | Arr vs  
  | Tuple vs -> eval_value_list useMap vs  