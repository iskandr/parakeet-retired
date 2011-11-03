(* pp: -parser o pa_macro.cmo *)

open Printf 
open Base

open SSA 
open Value 
 
type value = ArrayId.t Value.t  

let typeof v = DataManager.get_type v
let shapeof v = DataManager.get_shape v
let is_on_gpu v = DataManager.is_on_gpu v


let eval_value (valNode : SSA.value_node) : value =
  match valNode.value with
  | Var id -> DataManager.lookup id
  | Num n -> Value.Scalar n
  | _ ->
      let valStr = SSA.value_to_str valNode.value in
      failwith ("[eval_value] values of this type not implemented: " ^ valStr)
  
let eval_phi_node cond phiNode : unit = 
  let id = phiNode.phi_id in 
  let rhs = if cond then phiNode.phi_left else phiNode.phi_right in 
  let rhsVal = eval_value rhs in
  IFDEF DEBUG THEN 
    Printf.printf "\n===> [Eval] Phi assignment %s <- %s\n"
        (ID.to_str id)
        (Value.to_str rhsVal)
  ENDIF; 
  DataManager.set_binding id rhsVal 
    
let eval_phi_nodes cond phiNodes : unit =
  List.iter (eval_phi_node cond) phiNodes 
                        
let rec eval_block block = Block.iter_forward eval_stmt block 
and eval_stmt (stmtNode : SSA.stmt_node) : unit = 
  IFDEF DEBUG THEN 
    Printf.printf "\n===> EVAL: %s\n" (SSA.stmt_node_to_str stmtNode); 
  ENDIF; 
  match stmtNode.stmt with 
  | Set (ids, expNode) -> DataManager.set_bindings ids (eval_exp expNode) 
  | SetIdx (id, indices, rhs) -> assert false 
  | If (boolVal, tBlock, fBlock, phiNodes) ->
    let cond = Value.to_bool (eval_value boolVal) in 
    eval_block (if cond then tBlock else fBlock); 
    eval_phi_nodes cond phiNodes
      
  | WhileLoop (testBlock, testVal, body, header) ->
    eval_phi_nodes true header; 
    eval_block testBlock; 
    let cond = ref (eval_value testVal) in
    let niters = ref 0 in 
    while Value.to_bool !cond do 
      niters := !niters + 1; 
      eval_block body; 
      eval_phi_nodes false header; 
      eval_block testBlock; 
      cond := eval_value testVal; 
      IFDEF DEBUG THEN 
        Printf.printf "\n\n====> While Loop iteration %d\n%!" !niters; 
      ENDIF; 
    done
         
and eval_exp (expNode : SSA.exp_node) : value list = 
  match expNode.exp with 
  | Values valNodes -> List.map eval_value valNodes
  | Arr elts -> 
      [Value.Array (Array.of_list (List.map eval_value elts))]
  | Cast (t, valNode) when Type.is_scalar t -> 
      (match eval_value valNode with 
        | Value.Scalar n -> [Value.Scalar (ParNum.coerce_num n t)]
        | _ -> failwith "[eval] expected scalar"
      ) 
  | Cast (t, valNode) -> failwith "[eval] cast only implemented for scalars"
  
  (* first order array operators only *)
  | PrimApp (Prim.ArrayOp op, args) -> 
     [Scheduler.array_op op (List.map eval_value args)]
      
  | PrimApp (Prim.ScalarOp op, args) -> 
      let argVals = List.map eval_value args in 
      [eval_scalar_op op argVals]
      
  | Call (fnId, args) -> 
      let argVals = List.map eval_value args in
      let fundef = get_fundef fnId in 
      eval_app fundef argVals
      
  (* currently ignores axes *)
  | Adverb (op, 
    {closure_fn = fnId; closure_args = closureArgs},
    {args = args; axes=axes; init=init}) ->
      assert (init = None);
      let fn = FnManager.get_typed_function fnId in
      let fixed = List.map eval_value closureArgs in
      let arrays = List.map eval_value args in
      DataManager.enter_data_scope ();
      let results = match op with 
        | Prim.Map ->  Scheduler.map ~axes fn ~fixed arrays
        | Prim.AllPairs -> 
          assert (List.length arrays = 2); 
          let x = List.nth arrays 0 in 
          let y = List.nth arrays 1 in 
          Scheduler.all_pairs ~axes ~fixed x y
        | _ -> failwith "Adverb not implemented"   
      in 
      DataManager.exit_data_scope results;
      results
          
  | _ -> 
      failwith $ Printf.sprintf 
        "[eval_exp] no implementation for: %s\n"
        (SSA.exp_to_str expNode)
             
and eval_app fundef args =
  IFDEF DEBUG THEN 
    assert (List.length fundef.input_ids = List.length args); 
  ENDIF;
  DataManager.enter_scope (); 
  DataManager.set_bindings fundef.input_ids args;
  eval_block fundef.body;
  let outputs = List.map (DataManager.lookup P.memState) fundef.output_ids in 
  DataManager.exit_scope outputs; 
  outputs
    
and eval_scalar_op (op : Prim.scalar_op) (args : value list) =
(* whether the scalar is a GpuVal, a HostVal or an interpreter scalar, put *)
(* them all into hostvals                                                  *)
    let nums = List.map (DataManager.get_scalar P.memState) args in 
    match op, nums with 
    | Prim.Eq, [x; y] -> Value.of_bool (x = y) 
    | Prim.Neq, [x; y] -> Value.of_bool (x <> y) 
    | Prim.Lt, [x; y] ->
        Value.of_bool (ParNum.to_float x <= ParNum.to_float y) 
    | Prim.Lte, [x; y] -> 
        Value.of_bool (ParNum.to_float x <= ParNum.to_float y) 
    | Prim.Gt, [x; y] -> 
        Value.of_bool (ParNum.to_float x > ParNum.to_float y)
    | Prim.Gte, [x; y] -> 
        Value.of_bool (ParNum.to_float x >= ParNum.to_float y)
    | Prim.Div, [x; y] -> 
        Value.of_float (ParNum.to_float x /. ParNum.to_float y)
(* other math operations should return the same type as their inputs, so   *)
(* use the generic math eval function                                      *)
    | op, _ -> Value.Scalar (MathEval.eval_pqnum_op op nums)


let run fundef (hostData: Array.t Value.t list) =
  let vals = List.map (fun h -> DataManager.add_host memState h) hostData in 
  (* parameterize the evaluator by mutable global function table and       *)
	(* memory                                                                *)
  let outputVals = eval_app fundef vals in
  let results = List.map (DataManager.get_host memState) outputVals in 
  DataManager.flush_gpu memState; 
  results 