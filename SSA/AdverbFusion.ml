open Base
open SSA 

module StmtSet = Int.Set
module StmtMap = Int.Map 

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

let find_fusable_pred descriptor producerMap stmtMap =
  let candidateStmtIds = 
    Id.Map.find_list descriptor.consumes_list producerMap 
  in
  let candidateDescriptors : (adverb_descriptor * int) = 
    List.map (fun id -> id, StmtMap.find id stmtMap) candidateStmtIds 
  in 
  
  (* for now pick the first candidate possible always. 
     TODO: use a smarter heuristic (maybe most frequent candidate?)
  *) 
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

let describe = function  
  | Set(ids, 
      {  exp=App({value=Prim (Prim.ArrayOp op); value_type=opType}, args);
         exp_types = outputTypes 
      }) -> 
      let fnArgs, dataArgs = split_adverb_args op args in 
      { adverb = op; 
        adverb_type = opType; 
        function_args = fnArgs; 
        data_args = dataArgs; 
        produces_list = ids;
        produces_set = ID.Set.of_list ids; 
        (* only IDs corresponding to data, not including functions *)
        consumes_set = collect_vars dataArgs; 
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

let process_stmt adverbEnv graveyard stmtNode = match stmtNode.stmt with
  | Set _  ->
      let descriptor = describe stmtNode.stmt in 
      if find_fusable_pred descriptor adverbEnv then
        let combinedDescriptor = fuse predDescriptor descriptor in
        (* kill off the current statement and replace the predecessor *)  
        let graveyard' = StmtSet.add stmt.stmt_id graveyard in 
        let adverbEnv' = StmtMap.add predStmtId combinedDescriptor in 
        adverbEnv', graveyard'  
      else 
      StmtMap.add stmtNode.stmt_id desc, graveyard  
  | _ -> adverbEnv, graveyard   

let rec process_block adverbEnv graveyard = function 
  | [] -> adverbEnv, graveyard 
  | stmtNode::rest -> 
    let a', g' = process_stmt adverbEnv graveyard stmtNode in 
    process_block a' g' rest

(* once you've collected an environment of fused adverbs 
   and dead statements, rewrite the block 
*)  
let rec rewrite_block ?(acc=[]) adverbEnv graveyard = function
  | [] -> List.rev acc
  (* if a statement should be killed just don't cons it onto the accumulator *)
  | stmtNode::rest when StmtSet.mem stmtNode.id graveyard ->
      rewrite_block ~acc adverbEnv graveyard
  | ({stmt=Set _} as stmtNode)::rest ->
        let stmtNode' = 
          if StmtMap.mem stmtNode.stmt_id adverbEnv then 
            undescribe (StmtMap.find stmtNode.stmt_id adverbEnv)
          else
            stmtNode 
        in  
        rewrite_block ~acc:(stmtNode'::acc) adverbEnv graveyard 
  | ({stmt=If(cond, trueBlock, falseBlock, ifGate)} as stmtNode)::rest -> 
        let trueBlock' = rewrite_block adverbEnv graveyard trueBlock in 
        let falseBlock' = rewrite_block adverbEnv graveyard falseBlock in
        (* gate should still be valid since this optimization isn't allowed 
           to delete any identifiers, only move around where they are 
           created. Since some identifiers will, however, be redundant 
           it's best to follow up with a dead code elimination pass. 
        *)  
        {stmtNode with stmt = If(cond, trueBlock', falseBlock', ifGate) } 
  | _ -> failwith "not yet supported"             

let adverb_fusion block = 
  let adverbEnv, graveyard = process_block block in 
  rewrite_block adverbEnv graveyard block 
                 
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
 