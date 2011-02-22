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
      cost =  Imp.int 0;  
    }
      
  let value env valNode = match valNode.value with
    | Var id -> ID.Map.find id env.shapes
    | _ -> SymbolicShape.scalar 
    
  let exp  env expNode helpers  = match expNode.exp with 
    | Values vs -> helpers.eval_values env vs
    | Call (fnId, args) -> 
        let argShapes = List.map (value env) args in 
        P.call_cost fnId argShapes  
    | PrimApp of Prim.prim * value_nodes
      (* construction of arrays and values used by both typed and untyped ssa *) 
    | Arr of value_nodes
    | Cast of DynType.t * value_node  
    | Map of closure * value_nodes
    | Reduce of closure * closure * value_nodes * value_nodes 
    | Scan of closure * closure * value_nodes * value_nodes 
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

let rec symbolic_cost fnTable fundef = 
  let module Params = struct 
    let call_cost fnId shapes =
      let fundef' = FnTable.find fnId fnTable in 
      let costExpr = symbolic_cost fnTable fundef' in 
      (* substitute the symbolic shapes into symbolic costs, then simplify *) 
      let substCostExpr = ??? in 
      ImpSimplify.simplify_arith substCostExpr   
  end 
  in 
  let C = SSA_Analysis.MkEvaluator(CostAnalysis(Params)) in 
  let env = C.eval_fundef fundef in 
  env.cost 

(* cost of calling a function with specific shape arguments *) 
let eval_cost fnTable fundef = 
  let symbolicCost = symbolic_cost fnTable fundef in 
  (* ??? call evaluator for Imp expression with shape arguments? *) 
  
  