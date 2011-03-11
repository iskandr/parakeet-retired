open Base
open SSA
open Printf 
open SSA_Analysis

(*
   Evaluate the symbolic cost of calling a function, leaving the shape 
   of the input variables free/unspecified. 
   The cost function:
     - adverbs are costed in terms of their naive sequential interpretation 
     - all scalar operations have cost 1
     - assignments have cost 0
     - array creation has cost length(array)
     - the cost of a loop is just the cost of the body 
       (as if it were to only execute for one iteration)
     - if statements cost the maximum of either branch
   These symbolic costs are useful for later evaluating an expected cost for 
   the body of a GPU kernel, given some specific shape inputs.  
*)     


(* make cost analysis recursive via module param *) 
module type COST_ANALYSIS_PARAMS = sig 
  val call_cost : FnId.t -> SymbolicShape.shape list -> Imp.exp_node   
  val get_shape_env : SSA.fundef -> SymbolicShape.shape ID.Map.t 
end 

module CostAnalysis(P:COST_ANALYSIS_PARAMS) = struct 
  type value_info = SymbolicShape.shape  
  type exp_info = Imp.exp_node    
  type env = {
    shapes : SymbolicShape.shape ID.Map.t; 
    cost : Imp.exp_node 
  }  
  
  let iterative = false
  let dir = Forward 
  


  let init fundef = 
    { 
      shapes = P.get_shape_env fundef;  
      cost =  Imp.zero 
    } 
      
  let value env valNode = match valNode.value with
    | Var id -> ID.Map.find id env.shapes
    | _ -> SymbolicShape.scalar 
    
  let exp env expNode helpers  = match expNode.exp with 
    | Values vs -> Imp.int (List.length vs)
    | Call (fnId, args) -> 
        let argShapes = List.map (value env) args in 
        P.call_cost fnId argShapes  
    | Cast _
    | PrimApp (Prim.ScalarOp _, _) -> Imp.one
    | PrimApp _ -> Imp.infinity 
    | Arr elts -> Imp.int (List.length elts) 
    | Map (closure, args) -> 
        let closureArgShapes = List.map (value env) closure.SSA.closure_args in 
        let argShapes = List.map (value env) args in
        let maxDim, nestedShapes = SymbolicShape.split_max_rank argShapes in
        let fnId : FnId.t = closure.SSA.closure_fn in  
        let nestedArgs = closureArgShapes @ argShapes in
        let nestedCost : Imp.exp_node = P.call_cost  fnId nestedArgs in   
        Imp.mul_simplify maxDim nestedCost
          
    | Reduce (initClosure, closure, initArgs, args)   
    | Scan (initClosure, closure, initArgs, args) -> assert false   
    | App _ -> failwith "Unexpected untyped function application"   

  (* shape info already computed, assignments have zero cost in our model, *)
  (* so there's nothing to do for phi nodes *) 
  let phi_set _ _ _ = None 
  let phi_merge _ _ _ _ = None  
  
  let stmt env stmtNode helpers  = match stmtNode.stmt with 
    | Set(ids, rhs) -> 
      let rhsCost = exp env rhs helpers in 
      Some { env with cost = Imp.add_simplify rhsCost env.cost }   
       
    | SetIdx (id, indices, rhs) -> assert false 
    | If (testVal, tBlock, fBlock, phiNodes) -> 
        let tEnv, _  = helpers.eval_block env tBlock in 
        let fEnv, _  = helpers.eval_block env fBlock in 
        Some { env with cost = Imp.max_simplify tEnv.cost fEnv.cost } 
        
     (* testBlock, testVal, body, loop header, loop exit *)  
    | WhileLoop (testBlock, testVal, body, header, exit) -> 
      assert false     
end


let symCostCache : (FnId.t, Imp.exp_node) Hashtbl.t = Hashtbl.create 127 

let rec symbolic_seq_cost fnTable fundef = 
  try Hashtbl.find symCostCache fundef.SSA.fn_id 
  with _ -> 
    let module Params = struct
      
      let get_shape_env fundef = 
        ShapeInference.infer_normalized_shape_env fnTable fundef
        
      let call_cost fnId symShapes : Imp.exp_node =
        let fundef' = FnTable.find fnId fnTable in 
        (* cost expression with free input variables *) 
        let costExpr = symbolic_seq_cost fnTable fundef' in 
        (* substitute shapes for input IDs *) 
        let substEnv = 
          ID.Map.extend ID.Map.empty fundef'.SSA.input_ids symShapes 
        in 
        SymbolicShape.rewrite_dim substEnv costExpr
    end 
    in  
    let module C = CostAnalysis(Params) in 
    let module E = SSA_Analysis.MkEvaluator(C) in  
    let cost = (E.eval_fundef fundef).C.cost in 
    Hashtbl.add symCostCache fundef.SSA.fn_id cost; 
    cost   

let costCache : (FnId.t * Shape.t list, float) Hashtbl.t = Hashtbl.create 127 
let seq_cost fnTable fundef shapes =
  let key = fundef.SSA.fn_id, shapes in  
  try Hashtbl.find costCache key 
  with _ ->  
    let symCost = symbolic_seq_cost fnTable fundef in
    let shapeEnv = ID.Map.extend ID.Map.empty fundef.SSA.input_ids shapes in  
    let cost = ShapeEval.eval_exp_as_float shapeEnv symCost in 
    Hashtbl.add costCache key cost; 
    cost    