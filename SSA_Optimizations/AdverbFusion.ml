(* pp: -parser o pa_macro.cmo *)

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
  function_arg_ids : FnId.t list;  
  function_arg_types : DynType.t list; 
  data_args : SSA.value_node list;
   
  produces_list : ID.t list;
  (* keep both a list and set of ID produced to avoid wasteful conversions *) 
  produces_set : ID.Set.t; 
  consumes_set : ID.Set.t;
  consumes_list : ID.t list;   
} 

let count_matches  pred lst = 
  let rec aux counter = function
    | [] -> counter 
    | elt::rest -> if pred elt then aux (counter+1) rest else aux counter rest
  in 
  aux 0 lst 
  
(* is the fusion of succ (pred data) allowed? *) 
let compat_adverbs (pred:adverb_descriptor) (succ:adverb_descriptor) useCounts =
  (match pred.adverb, succ.adverb with
  | Prim.Map, Prim.Map 
  | Prim.Map, Prim.Reduce -> true
  | _ -> false)
  &&  
  (* 
     conservative condition: 
     all data produced by predecessor must be consumed by successor, 
     and not by any other statement
  *)
  (ID.Set.subset pred.produces_set succ.consumes_set)
  && 
  (ID.Set.for_all 
    (fun id -> 
      let nUsesInSucc = count_matches ((=) id) succ.consumes_list in 
      Hashtbl.find useCounts id = nUsesInSucc) 
    pred.produces_set
  )
  

let fuse_map_map 
      (pred:adverb_descriptor)
      (predFn : SSA.fundef)
      (succ:adverb_descriptor)
      (succFn : SSA.fundef) 
      (nestedSuccToPred : ID.t ID.Map.t)
      (deadNestedPredOutputs : ID.Set.t) =
        
  let keptSuccInputIds = 
    List.filter 
      (fun id -> not $ ID.Map.mem id nestedSuccToPred) 
      succFn.input_ids
  in 
  let inputIds = keptSuccInputIds @ predFn.input_ids in
  let inlineArgs = 
      List.map 
        (fun succId -> 
           let t = ID.Map.find succId succFn.tenv in    
           if ID.Map.mem succId nestedSuccToPred then 
             let predId = ID.Map.find succId nestedSuccToPred in 
             SSA.mk_var ~ty:t predId 
           else SSA.mk_var ~ty:t succId   
         )
        succFn.input_ids
  in
  let succBody, returnValsExp, typesList = Inline.do_inline succFn inlineArgs in  
  let body = 
    predFn.body @ succBody @ [SSA.mk_set succFn.output_ids returnValsExp] 
  in
  let tenv =
    List.fold_left (fun accEnv (id,t) -> ID.Map.add id t accEnv)
    (ID.Map.combine predFn.tenv succFn.tenv) 
    typesList
  in   
  (* exclude outputs we know are not used outside this fused computation *)
  let outputIds =
    List.filter 
      (fun id -> not $ ID.Set.mem id deadNestedPredOutputs)
      predFn.output_ids @ succFn.output_ids 
  in 
  SSA.mk_fundef ~body ~tenv ~input_ids:inputIds ~output_ids:outputIds

let fuse_map_reduce 
      (pred:adverb_descriptor)
      (predFn : SSA.fundef)
      (succ:adverb_descriptor)
      (succFn : SSA.fundef) 
      (nestedSuccToPred : ID.t ID.Map.t)
      (deadNestedPredOutputs : ID.Set.t) =
    
   (*Printf.printf "Dead: "; 
   ID.Set.iter (fun id -> Printf.printf " {%s} " (ID.to_str id)) deadNestedPredOutputs; 
   Printf.printf "\n";
  *)     
  (* inputs to the fused reduction will be the accumulators,
     any data not originating from the predecessor and 
     the inputs of the preceding map 
  *)
  let keptSuccInputIds = 
    List.filter 
      (fun id -> not $ ID.Map.mem id nestedSuccToPred) 
      succFn.input_ids
  in 
  
  let inputIds = keptSuccInputIds @ predFn.input_ids in
  (*
  IFDEF DEBUG THEN 
    Printf.printf "--- keptSuccInputIds: %s\n"
       (String.concat ", " (List.map ID.to_str keptSuccInputIds))
    ;   
    Printf.printf "--- inputIds: %s\n"
       (String.concat ", " (List.map ID.to_str inputIds))
    ;   
  ENDIF;
  *)   
  let inlineArgs =
    List.map 
      (fun succId ->
        let t = ID.Map.find succId succFn.tenv in    
        if ID.Map.mem succId nestedSuccToPred then
          SSA.mk_var ~ty:t $ ID.Map.find succId nestedSuccToPred 
        else SSA.mk_var ~ty:t succId
      )
      succFn.input_ids
      
  in      
  let succBody, returnValsExp, typesList = Inline.do_inline succFn inlineArgs in
  let body = 
    predFn.body @ succBody @ [SSA.mk_set succFn.output_ids returnValsExp] 
  in 
  let tenv =
    List.fold_left (fun accEnv (id,t) -> ID.Map.add id t accEnv) 
    (ID.Map.combine predFn.tenv succFn.tenv)
    typesList 
  in     
  (* exclude outputs we know are not used outside this fused computation *)
  let outputIds =
    List.filter 
      (fun id -> not $ ID.Set.mem id deadNestedPredOutputs)
      predFn.output_ids @ succFn.output_ids 
  in 
  SSA.mk_fundef ~body ~tenv ~input_ids:inputIds ~output_ids:outputIds        
         
let fuse 
      (fns : FnTable.t)  
      (use_counts : (ID.t, int) Hashtbl.t)
      (pred : adverb_descriptor) 
      (succ : adverb_descriptor) =
  
  let overlap = ID.Set.inter  pred.produces_set succ.consumes_set in 
  (* for now assume we only have 1 function argument *)  
  let fId, gId = match pred.function_arg_ids, succ.function_arg_ids with 
    | [fId], [gId] -> fId, gId
    | _ -> assert false
  in   
  let f = FnTable.find fId fns in 
  let g = FnTable.find gId fns in
  (* create a map of scalar outputs of f keyed by ids of vectors they go into *) 
  let predOuterToNested : ID.t ID.Map.t = 
    ID.Map.extend ID.Map.empty pred.produces_list f.output_ids
  in 
  let predNestedToOuter : ID.t ID.Map.t = 
    ID.Map.extend ID.Map.empty f.output_ids pred.produces_list 
  in 
  (* create a map of scalar input ids to g keyed by the ids 
     of the vectors they come from. 
     Ignore any outer inputs which aren't IDs since these
     can't possibly be data dependencies on the predecessor adverb. 
  *)
  let succNestedToOuter, succOuterToNested : ID.t ID.Map.t * ID.Set.t ID.Map.t =   
    List.fold_left2 
      (fun (nested2outer, outer2nested) scalarId inValNode ->
         match inValNode.value with 
         | Var vecId -> 
           let nestedVarsBindingThisVec = 
             ID.Map.find_default vecId outer2nested ID.Set.empty
           in  
           let nestedVarsBindingThisVec' = 
             ID.Set.add scalarId nestedVarsBindingThisVec 
           in  
           let outer2nested' = 
             ID.Map.add vecId nestedVarsBindingThisVec' outer2nested
           in  
           let nested2outer' = ID.Map.add scalarId vecId nested2outer in 
           (nested2outer', outer2nested')
         | _ ->  (nested2outer, outer2nested)
      )
      (ID.Map.empty, ID.Map.empty)
      g.input_ids 
      succ.data_args 
  in 
  (* pair the nested input to the successor with the nested output 
     from the predecessor function which produced the dependent value 
   *)
  let nestedSuccToPred : ID.t ID.Map.t =  
    List.fold_left   
      (fun accMap gInput  ->
         (* if an input to the nested function comes from 
            a non-constant source...
         *)
         if ID.Map.mem gInput succNestedToOuter then 
           let outerId = ID.Map.find gInput succNestedToOuter in
           (* if the predecessor produced this input... *) 
           if ID.Set.mem outerId pred.produces_set then     
             let fOutput = ID.Map.find outerId predOuterToNested in
             ID.Map.add gInput fOutput accMap
           else accMap
        else accMap   
      ) 
      ID.Map.empty
      g.input_ids            
  in 
  (* count how many times the successor uses overlap arguments, 
     so we can determine which will be dead after fusion 
   *)
  let succOverlapUseCounts = 
    List.fold_left
      (fun accMap inputNode  ->
        match inputNode.value with 
        | Var argId -> 
          let currCount : int = ID.Map.find_default argId accMap 0 in 
          ID.Map.add argId (currCount+1) accMap  
        | _ -> accMap
      )
      ID.Map.empty
      succ.data_args
  in 
  (* temporaries produced by predecessor are dead if they're not used 
     outside the arguments passed to the successor 
  *) 
  let deadTemps : ID.Set.t  = 
    ID.Set.filter 
      (fun id -> 
        let globalCount = Hashtbl.find use_counts id in 
        let argCount = ID.Map.find id succOverlapUseCounts in 
        assert (argCount <= globalCount);
        argCount = globalCount 
        
      )
      overlap
  in
  (* we know which vector outputs are dead, now just need to mark 
     the corresponding nested outputs as also being dead
  *)
  let deadNestedPred = 
    ID.Set.fold 
      (fun (outerId : ID.t) (accSet : ID.Set.t) ->
        ID.Set.add (ID.Map.find outerId predOuterToNested)  accSet
      )
      deadTemps
      ID.Set.empty 
  in      
  let fusedFns, finalAdverb = 
    match pred.adverb, succ.adverb with 
    | Prim.Map, Prim.Map -> 
      let fused = fuse_map_map pred f succ g nestedSuccToPred deadNestedPred in
      [fused], Prim.Map  
    | Prim.Map, Prim.Reduce -> 
      let fused = 
        fuse_map_reduce pred f succ g nestedSuccToPred deadNestedPred
      in 
      [fused], Prim.Reduce        
    | _ -> failwith $ Printf.sprintf 
             "[adverb_fusion->fuse] fusion of %s into %s not yet implemented"
             (Prim.array_op_to_str pred.adverb)
             (Prim.array_op_to_str succ.adverb)
  in 

  List.iter 
    (fun fundef -> 
      (*IFDEF DEBUG THEN 
        Printf.printf "Adding fn to table: %s\n"
          (SSA.fundef_to_str fundef); 
      ENDIF;
      *) 
      FnTable.add fundef fns) 
    fusedFns
  ; 
  let fusedIds = List.map (fun fundef -> fundef.fundef_id) fusedFns in
  let is_data_dependency vNode = match vNode.value with 
     | Var id -> not (ID.Set.mem id overlap) 
     | _ -> true
  in
  (* which data arguments are not internally produced by the fused 
     function 
  *)  
  let filteredDataArgs =
    List.filter is_data_dependency (succ.data_args @ pred.data_args )
  in
  (*
  IFDEF DEBUG THEN 
        Printf.printf "Filtered data args: %s\n" 
          (SSA.value_nodes_to_str filteredDataArgs);
  ENDIF;
  *)  
  let filteredTypes = 
    List.map (fun vNode -> vNode.value_type) filteredDataArgs 
  in
  (* we don't want to produce all the same vectors as the two operators 
     did individually-- some vectors get pruned using the deadTemps set
  *)
  let rec prune_produced_values ids types = match ids, types with 
    | id::restIds, ty::restTypes ->
        let restIds', restTypes' = prune_produced_values restIds restTypes in
        if ID.Set.mem id deadTemps then restIds', restTypes'
        else id::restIds', ty::restTypes'
    | [], [] -> [], [] 
    | _ -> assert false
  in 
  let predProducesTypes = DynType.fn_output_types pred.adverb_type in
  let succProducesTypes = DynType.fn_output_types succ.adverb_type in  
  let combinedProducesIds, combinedProducesTypes = 
    prune_produced_values 
      (pred.produces_list @ succ.produces_list)
      (predProducesTypes @ succProducesTypes)
  in 
  let combinedProducesTyEnv = 
    ID.Map.extend ID.Map.empty combinedProducesIds combinedProducesTypes
  in 
  (* consume all data except that which was produced by the pred and consumed
     by the succ--- this data should now be generated internally  
  *)  
  let combinedConsumes : ID.Set.t =
    ID.Set.diff
     (ID.Set.union pred.consumes_set succ.consumes_set)
     overlap
  in     
  let fusedFnTypes = List.map (fun fused -> fused.SSA.fundef_type) fusedFns in 
  {
    adverb = finalAdverb; 
    adverb_type = 
      DynType.FnT(fusedFnTypes @ filteredTypes, combinedProducesTypes); 
    function_arg_ids = fusedIds;
    function_arg_types = fusedFnTypes; 
    data_args = filteredDataArgs;  
    produces_list = combinedProducesIds; 
    produces_set = ID.Set.of_list combinedProducesIds;  
    consumes_set = combinedConsumes; 
    consumes_list = ID.Set.elements combinedConsumes;  
  }
  
  
let find_fusable_pred 
    ( use_counts : (ID.t, int) Hashtbl.t  )
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
        let compat = compat_adverbs otherDesc descriptor use_counts in
        if compat then Some (otherId, otherDesc) else try_pair rest        
  in try_pair candidateDescriptors    

let split_adverb_args op args =
  let fnArgs, dataArgs = match op, args with 
  | Prim.Reduce, f::rest
  | Prim.AllPairs, f::rest
  | Prim.Map, f::rest -> [f], rest
  | _ -> failwith "adverb not yet implemented" 
  in 
  let fnIds = List.map SSA.get_fn_id fnArgs in
  let fnTypes = List.map (fun vNode -> vNode.value_type) fnArgs in  
  fnIds, fnTypes, dataArgs 

let rec collect_var_list = function 
  | {value = SSA.Var id}::rest -> id::collect_var_list rest 
  | _::rest -> collect_var_list rest 
  | [] -> [] 

let exp_is_adverb = function 
  | App({value= Prim (Prim.ArrayOp op)}, _) -> Prim.is_adverb op
  | _ -> false  

let describe = function  
  | Set(ids, 
      {  exp=App({value=Prim (Prim.ArrayOp op); value_type=opType}, args);
         exp_types = outputTypes 
      }) when Prim.is_adverb op -> 
      let fnIds, fnTypes, dataArgs = split_adverb_args op args in
      let consumesList = collect_var_list dataArgs in  
      { adverb = op; 
        adverb_type = opType; 
        function_arg_ids = fnIds; 
        function_arg_types = fnTypes; 
        data_args = dataArgs; 
        produces_list = ids;
        produces_set = ID.Set.of_list ids; 
        (* only IDs corresponding to data, not including functions *)
        consumes_list = consumesList;   
        consumes_set = ID.Set.of_list consumesList;  
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
      (fun id t -> SSA.mk_globalfn ~ty:t id) 
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
    ~(use_counts : (ID.t, int) Hashtbl.t )
    ~(adverb_map : adverb_descriptor StmtMap.t) 
    ~(producer_map : StmtId.t ID.Map.t)
    ~(graveyard : StmtSet.t)
    ~(replaced : StmtSet.t)  
    (stmtNode : SSA.stmt_node)
     : (adverb_descriptor StmtMap.t * StmtId.t ID.Map.t * StmtSet.t * StmtSet.t) 
     = 
  match stmtNode.stmt with
  | Set (ids, rhs) when exp_is_adverb rhs.exp ->
      (
      try 
        let descriptor = describe stmtNode.stmt in 
        begin 
        match 
          find_fusable_pred use_counts descriptor adverb_map producer_map 
        with 
        | None ->
            let adverbMap' = 
              StmtMap.add stmtNode.stmt_id descriptor adverb_map 
            in 
            let producerMap' = 
              List.fold_left 
                (fun accMap id -> ID.Map.add id stmtNode.stmt_id accMap)
                producer_map
                ids
            in 
            adverbMap', producerMap', graveyard, replaced
        | Some (predStmtId, predDescriptor) ->
            (* 
               since we assume that no one used the predecessor's 
               data except the successor, we replace the successor 
               with the fused operation, and kill the predecessor 
            *)
             
            let graveyard' = StmtSet.add predStmtId graveyard in
            (* record that the current node should also be replaced *)
            let succStmtId = stmtNode.stmt_id in 
            let replaced' = StmtSet.add succStmtId replaced in   
            let combinedDescriptor : adverb_descriptor = 
              fuse fns use_counts predDescriptor descriptor 
            in
             
            let adverbMap' : adverb_descriptor StmtMap.t = 
              StmtMap.add succStmtId combinedDescriptor adverb_map 
            in
            
            (* We might also want to remove any data that died, 
               but since it isn't used by any other statement perhaps
               it's OK to leave it in the map.  
            *)   
            let producerMap' : StmtId.t ID.Map.t = 
              List.fold_left 
                (fun accMap id -> ID.Map.add id succStmtId accMap)
                producer_map
                combinedDescriptor.produces_list
            in  
            adverbMap', producerMap', graveyard', replaced' 
        end
        (* HACK HACK HACK: If we throw an exception, just keep rolling *) 
        with 
          | _ -> adverb_map, producer_map, graveyard, replaced
        )
  | _ -> adverb_map, producer_map, graveyard, replaced 

let rec process_block 
    (fns : FnTable.t)
    ~(use_counts : (ID.t, int) Hashtbl.t)
    ~(adverb_map : adverb_descriptor StmtMap.t)
    ~(producer_map : StmtId.t ID.Map.t) 
    ~(graveyard : StmtSet.t )
    ~(replaced : StmtSet.t)  = function 
  | [] -> adverb_map, producer_map, graveyard, replaced 
  | stmtNode::rest -> 
    let (a' : adverb_descriptor StmtMap.t), p', g', r' = 
      process_stmt 
        fns 
        ~use_counts 
        ~adverb_map 
        ~producer_map 
        ~graveyard 
        ~replaced 
        stmtNode 
    in 
    process_block 
      fns 
      ~use_counts 
      ~adverb_map:a' 
      ~producer_map:p' 
      ~graveyard:g' 
      ~replaced:r' 
      rest

(* once you've collected an environment of fused adverbs 
   and dead statements, rewrite the block 
*)  
let rec rewrite_block 
    (adverbMap : adverb_descriptor StmtMap.t) 
    (graveyard : StmtSet.t) 
    (replaced : StmtSet.t) = function
  | [] -> [], false
  (* if a statement should be killed just don't cons it onto the accumulator *)
  | stmtNode::rest when StmtSet.mem stmtNode.stmt_id graveyard ->
      rewrite_block adverbMap graveyard replaced rest
  | ({stmt=Set _} as stmtNode)::rest ->
        let rest', restChanged = 
          rewrite_block adverbMap graveyard replaced rest 
        in
        if StmtSet.mem stmtNode.stmt_id replaced then
          (* if stmt_id is in replacements then it has a new descriptor in 
             adverbMap 
          *)  
          let stmtNode' = 
            undescribe (StmtMap.find stmtNode.stmt_id adverbMap)
          in 
          stmtNode' :: rest', true 
        else
          stmtNode :: rest', restChanged
          
  | ({stmt=If(cond, trueBlock, falseBlock, ifGate)} as stmtNode)::rest -> 
        let trueBlock', trueChanged = 
          rewrite_block adverbMap graveyard replaced trueBlock 
        in 
        let falseBlock', falseChanged = 
          rewrite_block adverbMap graveyard replaced falseBlock 
        in
        (* gate should still be valid since this optimization isn't allowed 
           to delete any identifiers, only move around where they are 
           created. Since some identifiers will, however, be redundant 
           it's best to follow up with a dead code elimination pass. 
        *)  
        let stmtNode' = 
          {stmtNode with stmt = If(cond, trueBlock', falseBlock', ifGate) }
        in
        let rest', restChanged = 
          rewrite_block adverbMap graveyard replaced rest 
        in
        stmtNode'::rest', trueChanged || falseChanged || restChanged    
  | _ -> failwith "not yet supported"             

let optimize_block (fns : FnTable.t) useCounts block =
  let (adverbMap : adverb_descriptor StmtMap.t), _, graveyard, replaced = 
    process_block 
      fns
      ~use_counts:useCounts 
      ~adverb_map:StmtMap.empty 
      ~producer_map:ID.Map.empty 
      ~graveyard:StmtSet.empty 
      ~replaced:StmtSet.empty 
      block
  in 
  rewrite_block adverbMap graveyard replaced block 

let optimize_fundef (fns:FnTable.t) fundef =
  let useCounts = FindUseCounts.find_fundef_use_counts fundef in  
  let body', changed = optimize_block fns useCounts fundef.body in  
  {fundef with body = body' }, changed  
                                 
(* 
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
 