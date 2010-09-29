open Base
open SSA 

module StmtId = Int

module StmtSet = StmtId.Set
module StmtMap = StmtId.Map 

(* map every value produced by an adverb to the array operation, *) 
type adverb_descriptor = {
  adverb : Prim.array_op; 
  adverb_type : DynType.t; 
  (* assumes that all function args are unambiguous references to 
     known typed functions 
  *)    
  function_arg_ids : ID.t list;  
  function_arg_types : DynType.t list; 
  data_args : SSA.value_node list;
   
  produces_list : ID.t list;
  (* keep both a list and set of ID produced to avoid wasteful conversions *) 
  produces_set : ID.Set.t; 
  consumes_set : ID.Set.t;
  consumes_list : ID.t list;   
} 

(* is the fusion of a2 (a1 data) allowed? *) 
let compat_adverbs (a1:adverb_descriptor) (a2:adverb_descriptor) =
  match a1.adverb,a2.adverb with 
  | Prim.Map, Prim.Map -> 
    (* conservative condition is that all input data must be produced by 
       the predecessor. This is because we always replace the pred with the
       fused map--- but this might be impossible if there is a dependency on 
       data which is not created until later. 
    *)
    ID.Set.for_all (fun id -> ID.Set.mem id a1.produces_set) a2.consumes_set 
  | _ -> false 

let fuse_map_map 
      (pred:adverb_descriptor)
      (predFn : SSA.fundef)
      (succ:adverb_descriptor)
      (succFn : SSA.fundef) 
      (dependencyPairs : ID.t ID.Map.t) = predFn, DynType.BottomT
    (*let overlapList, succInputs = 
      List.partition (fun id -> ID.Set.mem id overlapSet) succFn.input_ids
    in   
    (* each overlap variable in the successor function needs to get its
       value from the corresponding output variable in the predecessor 
    *)  
    let overlapRhsVals =  
    let overlapDefs = SSA.mk_set overlapList 
    let body' = predFn.body @ overlapDefs @ succFn.body in
    let tenv' = PMap.combine predFn.tenv succFn.tenv in 
        
    {
      body = body'; 
      input_ids = predFn.input_ids @ succInputs;
      output_ids =  predFn.output_ids @ succFn.output_ids; 
    }   
    *)
let fuse 
      (fns : FnTable.t)  
      (pred : adverb_descriptor) 
      (succ : adverb_descriptor) =
  
  let overlap = ID.Set.inter  pred.produces_set succ.consumes_set in 
  let fusedFns, fusedTypes, finalAdverb = 
    match pred.adverb, pred.function_arg_ids, 
          succ.adverb, succ.function_arg_ids with 
    | Prim.Map, [fId], Prim.Map, [gId] -> 
        let f = FnTable.find fId fns in 
        let g = FnTable.find gId fns in
        (* create a map of scalar outputs of f 
           keyed by ids of vectors they go into *) 
        let overlapToNestedOutputs : ID.t ID.Map.t = 
          ID.Map.of_list (List.combine pred.produces_list f.output_ids)
        in 
        (* create a map of scalar input ids to g 
           keyed by the ids of the vectors they come from. 
           Ignore any outer inputs which aren't IDs since these
           can't possibly be data dependencies on the predecessor adverb. *)
        let overlapToNestedInputs : ID.t ID.Map.t =  
          List.fold_left2 
            (fun accMap scalarId inValNode ->
                match inValNode.value with 
                  | Var vecId -> ID.Map.add vecId scalarId accMap
                  | _ -> accMap
            )
            ID.Map.empty
            g.input_ids 
            succ.data_args 
        in 
        (* pair the nested input to the successor with the nested
           output from the predecessor function which produced
           the dependent value 
        *)
        let inputOutputPairs : ID.t ID.Map.t =  
          ID.Set.fold  
            (fun id accMap -> 
                let outputId = ID.Map.find id overlapToNestedOutputs in
                let inputId = ID.Map.find id overlapToNestedInputs in   
                ID.Map.add inputId outputId accMap 
            ) 
            overlap
            ID.Map.empty
            
        in 
        let fn, ty = fuse_map_map pred f succ g inputOutputPairs in 
        [fn], [ty], Prim.Map 
    | _ -> failwith "[adverb_fusion->fuse] adverb pair not yet implemented"
  in 
  let fusedIds = List.map (fun fundef -> FnTable.add fundef fns) fusedFns in
  let is_data_dependency vNode = match vNode.value with 
     | Var id -> not (ID.Set.mem id overlap) 
     | _ -> true
  in
  (* which data arguments are not internally produced by the fused 
     function 
  *)  
  let filteredDataArgs =
    List.filter is_data_dependency (pred.data_args @ succ.data_args)
  in 
  let filteredTypes = 
    List.map (fun vNode -> vNode.value_type) filteredDataArgs 
  in 
  (* assume that every value is produced by only one statement, 
     we never duplicate work 
  *) 
  let combinedProducesIds : ID.t list = 
    pred.produces_list @ succ.produces_list 
  in 
  let combinedProducesTypes : DynType.t list = 
    (DynType.fn_output_types pred.adverb_type) @ 
    (DynType.fn_output_types succ.adverb_type) 
  in   
  (* consume all data except that which was produced by the pred and consumed
     by the succ--- this data should now be generated internally  
  *)  
  let combinedConsumes : ID.Set.t =
    ID.Set.diff
     (ID.Set.union pred.consumes_set succ.consumes_set)
     overlap
  in     
  {
    adverb = finalAdverb; 
    adverb_type = 
      DynType.FnT(fusedTypes @ filteredTypes, combinedProducesTypes); 
    function_arg_ids = fusedIds;
    function_arg_types = fusedTypes; 
    data_args = filteredDataArgs;  
    produces_list = combinedProducesIds; 
    produces_set = ID.Set.of_list combinedProducesIds;  
    consumes_set = combinedConsumes; 
    consumes_list = ID.Set.elements combinedConsumes;  
  }
  
  
let find_fusable_pred 
    ( descriptor : adverb_descriptor)
    ( stmtMap : adverb_descriptor StmtMap.t)
    ( producerMap : StmtId.t ID.Map.t) =
      
  let candidateStmtIds = 
    ID.Map.find_list descriptor.consumes_list producerMap 
  in
  let candidateDescriptors : ( int * adverb_descriptor ) list = 
    List.map (fun id -> id, StmtMap.find id stmtMap) candidateStmtIds 
  in 
  let rec try_pair = function 
    | [] -> None  
    | (otherId, otherDesc)::rest ->
        if compat_adverbs otherDesc descriptor then Some (otherId, otherDesc) 
        else try_pair rest    
  in try_pair candidateDescriptors    



let split_adverb_args op args =
  let fnArgs, dataArgs = match op, args with 
  | Prim.Reduce, f::rest
  | Prim.AllPairs, f::rest
  | Prim.Map, f::rest -> [f], rest
  | _ -> failwith "adverb not yet implemented" 
  in 
  let fnIds = List.map SSA.get_id fnArgs in
  let fnTypes = List.map (fun vNode -> vNode.value_type) fnArgs in  
  fnIds, fnTypes, dataArgs 
    
let rec collect_vars = function 
  | {value = SSA.Var id}::rest -> 
      let set = collect_vars rest in        
      ID.Set.add id set 
  | _::rest -> collect_vars rest 
  | [] -> ID.Set.empty 

let exp_is_adverb = function 
  | App({value= Prim (Prim.ArrayOp op)}, _) -> Prim.is_adverb op
  | _ -> false  

let describe = function  
  | Set(ids, 
      {  exp=App({value=Prim (Prim.ArrayOp op); value_type=opType}, args);
         exp_types = outputTypes 
      }) when Prim.is_adverb op -> 
      let fnIds, fnTypes, dataArgs = split_adverb_args op args in
      let consumesSet = collect_vars dataArgs in  
      { adverb = op; 
        adverb_type = opType; 
        function_arg_ids = fnIds; 
        function_arg_types = fnTypes; 
        data_args = dataArgs; 
        produces_list = ids;
        produces_set = ID.Set.of_list ids; 
        (* only IDs corresponding to data, not including functions *)
        consumes_set = consumesSet;  
        consumes_list = ID.Set.to_list consumesSet; 
        (*data_arg_types = List.map (fun v -> v.value_type) dataArgs; 
        adverb_output_types =  outputTypes*)
      }
 | _ -> 
    failwith "[adverb_fusion] can only describe application of array operators"    
  
(* go from an adverb description to a statement *) 
let undescribe desc = 
  let adverbNode = 
    SSA.mk_val ~ty:desc.adverb_type (SSA.Prim (Prim.ArrayOp desc.adverb))
  in 
  let fnArgs = 
    List.map2 
      (fun id t -> SSA.mk_var ~ty:t id) 
      desc.function_arg_ids 
      desc.function_arg_types
  in 
  let args = fnArgs  @ desc.data_args in 
  let appNode = 
    SSA.mk_app ~types:(DynType.fn_output_types desc.adverb_type) adverbNode args 
  in 
  SSA.mk_stmt (Set(desc.produces_list, appNode))     

let process_stmt 
    (fns : FnTable.t)
    (adverbMap : adverb_descriptor StmtMap.t) 
    (producerMap : StmtId.t ID.Map.t)
    (graveyard : StmtSet.t) 
    (stmtNode : SSA.stmt_node)
     : (adverb_descriptor StmtMap.t * StmtId.t ID.Map.t * StmtSet.t)  = 
  match stmtNode.stmt with
  | Set (ids, rhs) when exp_is_adverb rhs.exp ->
      let descriptor = describe stmtNode.stmt in 
      begin match find_fusable_pred descriptor adverbMap producerMap with 
        | None ->
            let adverbMap' = 
              StmtMap.add stmtNode.stmt_id descriptor adverbMap 
            in 
            let producerMap' = 
              List.fold_left 
                (fun accMap id -> ID.Map.add id stmtNode.stmt_id accMap)
                producerMap
                ids
            in 
            adverbMap', producerMap', graveyard
        | Some (predStmtId, predDescriptor) ->
            (* kill off the current statement and replace the predecessor *)
            let graveyard' = StmtSet.add stmtNode.stmt_id graveyard in 
            let combinedDescriptor : adverb_descriptor = 
              fuse fns predDescriptor descriptor 
            in
            let adverbMap' : adverb_descriptor StmtMap.t = 
              StmtMap.add predStmtId combinedDescriptor adverbMap 
            in
            let producerMap' : StmtId.t ID.Map.t = 
              List.fold_left 
                (fun accMap id -> ID.Map.add id predStmtId accMap)
                producerMap
                predDescriptor.produces_list
            in  
            adverbMap', producerMap', graveyard'  
      end
          
  | _ -> adverbMap, producerMap, graveyard   

let rec process_block 
    (fns : FnTable.t)
    (adverbMap : adverb_descriptor StmtMap.t)
    (producerMap : StmtId.t ID.Map.t) 
    (graveyard : StmtSet.t ) = function 
  | [] -> adverbMap, producerMap, graveyard 
  | stmtNode::rest -> 
    let a', p', g' = 
      process_stmt fns adverbMap producerMap graveyard stmtNode 
    in 
    process_block fns a' p' g' rest

(* once you've collected an environment of fused adverbs 
   and dead statements, rewrite the block 
*)  
let rec rewrite_block adverbMap graveyard = function
  | [] -> [], false
  (* if a statement should be killed just don't cons it onto the accumulator *)
  | stmtNode::rest when StmtSet.mem stmtNode.stmt_id graveyard ->
    rewrite_block adverbMap graveyard rest
  | ({stmt=Set _} as stmtNode)::rest ->
        let rest', restChanged = rewrite_block adverbMap graveyard rest in
        if StmtMap.mem stmtNode.stmt_id adverbMap then 
          let stmtNode' = 
            undescribe (StmtMap.find stmtNode.stmt_id adverbMap)
          in 
          stmtNode' :: rest', true 
        else
          stmtNode :: rest, restChanged
          
  | ({stmt=If(cond, trueBlock, falseBlock, ifGate)} as stmtNode)::rest -> 
        let trueBlock', trueChanged = 
          rewrite_block adverbMap graveyard trueBlock 
        in 
        let falseBlock', falseChanged = 
          rewrite_block adverbMap graveyard falseBlock 
        in
        (* gate should still be valid since this optimization isn't allowed 
           to delete any identifiers, only move around where they are 
           created. Since some identifiers will, however, be redundant 
           it's best to follow up with a dead code elimination pass. 
        *)  
        let stmtNode' = 
          {stmtNode with stmt = If(cond, trueBlock', falseBlock', ifGate) }
        in
        let rest', restChanged = rewrite_block adverbMap graveyard rest in
        stmtNode'::rest', trueChanged || falseChanged || restChanged    
  | _ -> failwith "not yet supported"             

let optimize_block (fns : FnTable.t) block = 
  let (adverbMap : adverb_descriptor StmtMap.t), _, (graveyard :StmtSet.t) = 
    process_block fns StmtMap.empty ID.Map.empty StmtSet.empty block 
  in 
  rewrite_block adverbMap graveyard block 

let optimize_fundef (fns:FnTable.t) fundef = 
  let body', changed = optimize_block fns fundef.body in  
  {fundef with body = body' }, changed  
                                 
(* sample program: 
      b, c = map(f, a) 
      d = map(g, b)
      e = map(h, c) 
   (==>)  
      b,c,d = map(f.g, a) 
      e = map(h, c) 
   (==>)
      b,c,d,e = map( (f.g).e, a) 

   - Do we have a good definition for multi-argument function composition? 
   - algorithm? 
        Let s1, s2 be two assignments with MAP adverbs on the rhs. 
        Draw s1, s2 nondeterministically from set of all adverb statements. 
        If consumes(s2) /\ produces(s1) =/= {} then 
           let f' = fn(s1) . fn(s2) 
        erase s2 and replace s1 in the original program with s' 
        
   "Replace in the program"... means what? 
   - replace 1st variable in your adverb map
   - mark the 2nd variable as deleted (maybe just remove it from the map? 
     or add it to a graveyard?) 
   -    
                        
 *) 
 