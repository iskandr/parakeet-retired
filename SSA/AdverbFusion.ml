
open SSA 

module IdSet = Set.Make(ID)
module StmtEnv = Map.Make(struct type t = int let compare = compare end)
module BoolSet = Set.Make(struct type t = bool let compare = compare end) 

(* map every value produced by an adverb to the array operation, *) 
type adverb_descriptor = {
  adverb : Prim.array_op; 
  function_args : SSA.value_node list;  
  data_args : SSA.value_node list; 
  produces_data : IdSet.t;
  consumes_data : IdSet.t; 
} 

let split_adverb_args op args = match op, args with 
  | Prim.Reduce, f::rest
  | Prim.AllPairs, f::rest
  | Prim.Map, f::rest -> f, rest
  | _ -> failwith "adverb not yet implemented" 
   
let rec collect_vars = function 
  | {value = SSA.Var id}::rest -> 
      let set = collect_vars rest in        
      IdSet.add id set 
  | _::rest -> collect_vars rest 
  | [] -> IdSet.empty 

let describe outputIds op args =
  let fnArgs, dataArgs = split_adverb_args op args in 
  let inputIds = collect_vars dataArgs in
  { adverb = op; function_args = fnArgs; data_args = dataArgs; 
    produces_data = outputIds;
    consumes_data = inputIds
  }    

let process_stmt adverbEnv graveyard stmtNode = match stmtNode.stmt with
  | Set(ids, {exp=App({value=SSA.Prim (Prim.ArrayOp op)}, args)}) ->
      let descriptor = describe ids op args in 
      if fusion_possible descriptor adverbEnv then
        let predStmtId, predDescriptor = 
          choose_pred_adverb descriptor adverbEnv 
        in 
        let combinedDescriptor = fuse predDescriptor descriptor in
        (* kill off the current statement and replace the predecessor *)  
        let graveyard' = BoolSet.add stmt.stmt_id graveyard in 
        let adverbEnv' = StmtEnv.add predStmtId combinedDescriptor in 
        adverbEnv', graveyard'  
      else 
      StmtEnv.add stmtNode.stmt_id desc, graveyard  
  | _ -> adverbEnv, graveyard   

let rec process_block adverbEnv graveyard = function 
  | [] -> adverbEnv, graveyard 
  | stmtNode::rest -> 
    let a', g' = process_stmt adverbEnv graveyard stmtNode in 
    process_block a' g' rest

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
 