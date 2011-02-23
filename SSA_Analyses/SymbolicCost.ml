open Base
open SSA
open Printf 
open SSA_Analysis

type symbolic_cost = Imp.exp_node

(* make cost analysis recursive via module param *) 
module type COST_ANALYSIS_PARAMS = sig 
  val call_cost : FnId.t -> SymbolicShape.shape list -> symbolic_cost  
end 

module CostAnalysis(P:COST_ANALYSIS_PARAMS) = struct 
  
  type value_info = SymbolicShape.shape  
  type exp_info = symbolic_cost    
  type env = {
    shapes : SymbolicShape.shape ID.Map.t; 
    cost : symbolic_cost 
  }  
  
  let iterative = false
  let dir = Forward 
  

  let init fundef = 
    { 
      shapes =  ShapeInference.infer_ssa_shape_env fundef; 
      cost =  Imp.zero 
    } 
      
  let value env valNode = match valNode.value with
    | Var id -> ID.Map.find id env.shapes
    | _ -> SymbolicShape.scalar 
    
  let exp  env expNode helpers  = match expNode.exp with 
    | Values vs -> helpers.eval_values env vs
    | Call (fnId, args) -> 
        let argShapes = List.map (value env) args in 
        P.call_cost fnId argShapes  
    | Cast _
    | PrimApp (Prim.ScalarOp _, _) -> Imp.one
    | Arr elts -> Imp.int (List.length elts) 
    | Map (closure, args) -> 
    | Reduce (initClosure, closure, initArgs, args)   
    | Scan (initClosure, closure, initArgs, args) ->  
       
    | App _ -> failwith "Unexpected untyped function application"   
 
      

  (* shape info already computed, assignments have zero cost in our model, *)
  (* so there's nothing to do for phi nodes *) 
  let phi env leftEnv rightEnv phiNode = None 
  
  let stmt env stmtNode helpers  = match stmtNode.stmt with 
    | Set(ids, rhs) -> assert false 
    | SetIdx of ID.t * value_nodes * value_node
    | If of value_node * block * block * phi_nodes
     (* testBlock, testVal, body, loop header, loop exit *)  
    | WhileLoop of block * value_node * block * phi_nodes * phi_nodes    
end


let costCache : (FnId.t, symbolic_cost) Hashtbl.t = Hahstbl.create 127 

let rec symbolic_cost fnTable fundef = 
  try Hashtbl.find costCache fundef.SSA.fn_id 
  with _ -> 
    let module Params = struct 
      let call_cost fnId shapes =
        let fundef' = FnTable.find fnId fnTable in 
        (* cost expression with free input variables *) 
        let costExpr = symbolic_cost fnTable fundef' in 
        (* substitute shapes for input IDs *) 
        let substEnv = ID.Map.extend ID.Map.empty fundef'.SSA.input_ids shapes in 
        SymbolicShape.rewrite_dim substEnv costExpr
    end 
    in 
    let C = SSA_Analysis.MkEvaluator(CostAnalysis(Params)) in 
    let cost = (C.eval_fundef fundef).cost in 
    Hashtbl.add costCache fundef.SSA.fn_id cost; 
    cost   
