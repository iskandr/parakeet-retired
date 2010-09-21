open Base
open SSA 

module StmtId = Int

module StmtSet = StmtId.Set
module StmtMap = StmtId.Map 

(* map every value produced by an adverb to the array operation, *) 
type adverb_descriptor = {
  adverb : Prim.array_op; 
  adverb_type : DynType.t; 
  function_args : SSA.value_node list;  
  data_args : SSA.value_node list; 
  produces_list : ID.t list;
  (* keep both a list and set of ID produced to avoid wasteful conversions *) 
  produces_set : ID.Set.t; 
  consumes_set : ID.Set.t;
  consumes_list : ID.t list; 
  data_arg_types : DynType.t list; 
  adverb_output_types : DynType.t list;  
} 

(* is the fusion of a2 (a1 data) allowed? *) 
let compat_adverbs a1 a2 = match (a1,a2) with 
  | Prim.Map, Prim.Map -> true
  | _ -> false 

(* TODO: fix this *) 
let fuse (pred : adverb_descriptor) (succ : adverb_descriptor) = pred 

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
        if compat_adverbs otherDesc.adverb descriptor.adverb then
           Some (otherId, otherDesc) 
        else try_pair rest    
  in try_pair candidateDescriptors    

let split_adverb_args op args = match op, args with 
  | Prim.Reduce, f::rest
  | Prim.AllPairs, f::rest
  | Prim.Map, f::rest -> [f], rest
  | _ -> failwith "adverb not yet implemented" 
   
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
      let fnArgs, dataArgs = split_adverb_args op args in
      let consumesSet = collect_vars dataArgs in  
      { adverb = op; 
        adverb_type = opType; 
        function_args = fnArgs; 
        data_args = dataArgs; 
        produces_list = ids;
        produces_set = ID.Set.of_list ids; 
        (* only IDs corresponding to data, not including functions *)
        consumes_set = consumesSet;  
        consumes_list = ID.Set.to_list consumesSet; 
        data_arg_types = List.map (fun v -> v.value_type) dataArgs; 
        adverb_output_types =  outputTypes
      }
 | _ -> 
    failwith "[adverb_fusion] can only describe application of array operators"    
  
(* go from an adverb description to a statement *) 
let undescribe desc = 
  let adverbNode = 
    SSA.mk_val ~ty:desc.adverb_type (SSA.Prim (Prim.ArrayOp desc.adverb))
  in 
  let args = desc.function_args @ desc.data_args in 
  let appNode = SSA.mk_app ~types:desc.adverb_output_types adverbNode args in 
  SSA.mk_stmt (Set(desc.produces_list, appNode))     

let process_stmt 
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
              fuse predDescriptor descriptor 
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
    (adverbMap : adverb_descriptor StmtMap.t)
    (producerMap : StmtId.t ID.Map.t) 
    (graveyard : StmtSet.t ) = function 
  | [] -> adverbMap, producerMap, graveyard 
  | stmtNode::rest -> 
    let a', p', g' = process_stmt adverbMap producerMap graveyard stmtNode in 
    process_block a' p' g' rest

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

let optimize_block block = 
  let (adverbMap : adverb_descriptor StmtMap.t), _, (graveyard :StmtSet.t) = 
    process_block StmtMap.empty ID.Map.empty StmtSet.empty block 
  in 
  rewrite_block adverbMap graveyard block 

let optimize_fundef fundef = 
  let body', changed = optimize_block fundef.body in  
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
 