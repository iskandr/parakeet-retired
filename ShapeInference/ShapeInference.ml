(* pp: -parser o pa_macro.cmo *)

let _ = Printexc.record_backtrace true 

open Base 
open Base
open SSA
open SSA_Analysis 
open SymbolicShape
(* takes an SSA function and a map of its IDs to their counterparts in some
   future Imp function. Infers an Imp.exp array corresponding to each
   variable's shape 
*)  

exception ShapeInferenceFailure of string

module type PARAMS = sig 
  val output_shapes : FnId.t -> shape list -> shape list  
end 

module ShapeAnalysis (P: PARAMS) =  struct
    type value_info = shape
    type exp_info = value_info list    
    type env = value_info ID.Map.t 
    
    let dir = Forward
    
     let clone_env env = env
        
    (* should analysis be repeated until environment stops changing? *) 
    let iterative = true  
  
    (* TODO: add memoization on function ID here *) 
    let init fundef = 
      List.fold_left 
        (fun accEnv id -> 
          let varNode = Imp.var ~t:(ID.Map.find id fundef.SSA.tenv) id in 
          ID.Map.add id (all_dims varNode)  accEnv
        )
        ID.Map.empty 
        fundef.input_ids 
   
    let value env valNode = match valNode.value with 
      | SSA.Var id -> all_dims (Imp.var ~t:valNode.value_type id)  
      | _ -> [] (* empty list indicates a scalar *) 
    
    let phi_set env id shape = 
      if ID.Map.mem id env then ( 
        let oldShape = ID.Map.find id env in 
        if shape <> oldShape then failwith "Shape error"
        else None 
      )
      else Some (ID.Map.add id shape env)
   
    let phi_merge env id leftShape rightShape = 
      if leftShape <> rightShape then failwith "Shape error";
      phi_set env id leftShape 
      
    
    let exp env expNode helpers = 
      let get_shapes args = List.map (value env) args in  
      match expNode.exp with
      | SSA.Call(fnId, args) -> 
          raise (ShapeInferenceFailure "unexpected function call")

      | SSA.PrimApp (Prim.ArrayOp Prim.Index, array::indices) ->
        let arrayShape = value env array in
        let nIndices = List.length indices in 
        let indexShapes = get_shapes indices in 
        if List.for_all SymbolicShape.is_scalar indexShapes then (  
              
        (* for now assume slicing can only happen along the 
           outermost dimensions and only by scalar indices 
         *)  
          assert (SymbolicShape.rank arrayShape > nIndices);
          [List.drop nIndices arrayShape]
        )
        (* for now we're also allowing index vectors, though this really 
           ought to become a map 
         *) 
        else ( 
          assert (nIndices = 1); 
          let idxShape = List.hd indexShapes  in  
          [SymbolicShape.concat idxShape (SymbolicShape.peel_shape arrayShape)]
             
        )
      | SSA.PrimApp (Prim.ArrayOp Prim.Where, [array]) ->
        (* an upper bound would be: [value env array], but 
           for now fail if we can't be exact 
         *)
         let arrayShape = value env array in
         let n = SymbolicShape.nelts arrayShape in
         [[n]]
         (*raise (ShapeInferenceFailure "Can't infer output shape of WHERE prim")*)
         
      | SSA.PrimApp (Prim.ArrayOp Prim.DimSize, [array; dim]) -> [[]] 
      | SSA.PrimApp (Prim.ScalarOp _, args) when 
        List.for_all (fun arg -> DynType.is_scalar arg.value_type) args -> [[]]
      | SSA.Arr elts ->
        let eltShapes = List.map (value env) elts in
        (* TODO: check that elt shapes actually match each other *) 
        let n = List.length eltShapes in
        [Imp.int n :: (List.hd eltShapes)]             
      | SSA.Cast (t, v) ->  [value env v] 
      | SSA.Values vs -> List.map (value env) vs  
      | SSA.Map(closure, args) -> 
          let closArgShapes : shape list = 
            List.map (value env) closure.closure_args 
          in 
          let argShapes = get_shapes args in
          let maxDim, eltShapes = SymbolicShape.split_max_rank argShapes in 
          let eltInShapes = (closArgShapes @ eltShapes) in 
          let eltOutShapes = P.output_shapes closure.closure_fn eltInShapes in  
          (* collect only args of maximal rank *) 
          let vecOutShapes = 
            List.map (fun outShape -> maxDim :: outShape) eltOutShapes
          in  
          IFDEF DEBUG THEN 
            Printf.printf "\t MAP (%s)(%s)\n"
              (SSA.closure_to_str closure)
              (SSA.value_nodes_to_str args)
            ; 
         
            Printf.printf "\t\t fn input shapes: %s\n" 
              (shapes_to_str eltInShapes)
            ; 
            Printf.printf "\t\t fn output shapes: %s\n" 
              (shapes_to_str eltOutShapes)
            ; 
            Printf.printf "\t\t maxDim: %s\n" (Imp.exp_node_to_str maxDim);
            Printf.printf "\t\t final output shapes: %s\n"
              (shapes_to_str vecOutShapes); 
          ENDIF;  
          vecOutShapes
          
      | SSA.Reduce(initClos, clos, initArgs, args) -> 
          let initClosArgShapes = get_shapes initClos.closure_args in 
          let initShapes  = get_shapes initArgs in 
          let argShapes = get_shapes args in
          let _, eltShapes = SymbolicShape.split_max_rank argShapes in 
          let allInputs = initClosArgShapes @ initShapes @ eltShapes in  
          P.output_shapes initClos.SSA.closure_fn allInputs    
      | other -> 
          let expStr = SSA.exp_to_str expNode in 
          failwith (Printf.sprintf "[shape_infer] not implemented: %s\n" expStr) 
      
      let stmt env stmtNode helpers = match stmtNode.stmt with
      | Set(ids, rhs) ->
          let rhsShapes = exp env rhs helpers in
          IFDEF DEBUG THEN 
            if List.length ids <> List.length rhsShapes then 
              failwith ("Shape inference error in stmt '" ^
                        (SSA.stmt_node_to_str stmtNode) ^ 
                        "': number of IDs must match number of rhs shapes");
          ENDIF;  
          let env' =
            List.fold_left2 
              (fun env id shape -> ID.Map.add id shape env)
              env
              ids
              rhsShapes 
          in Some env' 
      | _ -> None    
end


(* given a dim expression (one elt of a shape) which may reference 
   intermediate variables, "normalize" it to only refer to input variables
*)   
let rec normalize_dim inputSet rawShapeEnv normalizedEnv expNode
        : Imp.exp_node * shape ID.Map.t  = 
  match expNode.Imp.exp with  
  | Imp.Op (op, t, args) ->
      (* a list of dims and a shape are the same thing, so just reuse the
         normalize_shape function for a list of dim arguments 
      *) 
      let args', normalizedEnv' = 
        normalize_shape inputSet rawShapeEnv normalizedEnv args 
      in 
      let expNode' = {expNode with Imp.exp = Imp.Op(op, t, args') } in
      ImpSimplify.simplify_arith expNode', normalizedEnv'   
  | Imp.Const n -> expNode, normalizedEnv   
  | Imp.DimSize (d, {Imp.exp=Imp.Var id}) ->
      if ID.Set.mem id inputSet then expNode, normalizedEnv 
      else  
        let shape, normalizedEnv' = 
          if ID.Map.mem id normalizedEnv then 
            (debug $ (ID.to_str id) ^ " already normalized"; 
            ID.Map.find id normalizedEnv, normalizedEnv)  
          else   
            (* if some local variable's shape has not yet been normalized,
               do so recursively 
            *) 
            
            let rawShape = ID.Map.find id rawShapeEnv in
            let normalizedShape, normalizedEnv' = 
              normalize_shape inputSet rawShapeEnv normalizedEnv rawShape 
            in
            normalizedShape, ID.Map.add id normalizedShape normalizedEnv'
        in  
        SymbolicShape.get_dim shape d, normalizedEnv' 
          
  | Imp.Var id -> failwith "variables should only appear in dimsize expressions"
  | _ ->failwith ("unexpected Imp expression: " ^ (Imp.exp_node_to_str expNode))
  
and normalize_shape inputSet rawShapeEnv normalizedEnv shape 
    : shape * shape ID.Map.t  =
  let foldFn (revDims, normalizedEnv) currDim = 
    let currDim', normalizedEnv' = 
      normalize_dim inputSet rawShapeEnv normalizedEnv currDim 
    in  
    currDim' :: revDims, normalizedEnv' 
  in 
  
  let revShape, env' = List.fold_left foldFn ([], normalizedEnv) shape in
  List.rev revShape, env' 

let rec normalize_shape_list inputSet rawShapeEnv normalizedEnv = function 
  | [] -> [], normalizedEnv 
  | shape::rest -> 
      let rest', normalizedEnv' = 
        normalize_shape_list inputSet rawShapeEnv normalizedEnv rest 
      in
      let shape', normalizedEnv'' =
        normalize_shape inputSet rawShapeEnv normalizedEnv shape
      in 
      shape'::rest', normalizedEnv''     



(* cache the normalized output shape expressions of each function. "normalized" 
   means the expressions refer to input IDs, which need to be replaced by some 
   input expression later
*)
let normalizedOutputShapeCache : (FnId.t, shape list) Hashtbl.t = 
  Hashtbl.create 127
  
let shapeEnvCache : (FnId.t, SymbolicShape.env) Hashtbl.t = Hashtbl.create 127
(* like the shape env cache, except the shape expressions have all been 
   normalize to refer only to input variables and not intermediates
*) 
let normalizedShapeEnvCache : (FnId.t, SymbolicShape.env) Hashtbl.t = 
  Hashtbl.create 127 
  
  
let rec infer_shape_env (fnTable:FnTable.t) (fundef : SSA.fundef) =
  let fnId = fundef.SSA.fn_id in
  IFDEF DEBUG THEN 
    Printf.printf "Looking up shape env for %s\n" (FnId.to_str fnId);
  ENDIF;  
  try Hashtbl.find shapeEnvCache fnId  
  with _ -> 
    let module Params : PARAMS = struct 
      let output_shapes fnId argShapes =
        let fundef = FnTable.find fnId fnTable in 
        infer_call_result_shapes fnTable fundef argShapes 
    end 
    in 
    let module ShapeEval = SSA_Analysis.MkEvaluator(ShapeAnalysis(Params)) in 
    let shapeEnv = ShapeEval.eval_fundef fundef in
    IFDEF DEBUG THEN
      Printf.printf "Inferred shape environment for %s : %s -> %s:\n"
        (FnId.to_str fnId)
        (DynType.type_list_to_str fundef.SSA.fn_input_types)
        (DynType.type_list_to_str fundef.SSA.fn_output_types)
      ;  
      ID.Map.iter 
        (fun id shape -> 
          Printf.printf " -- %s : %s\n" 
            (ID.to_str id) 
            (Imp.exp_node_list_to_str shape)
        )
        shapeEnv;
    ENDIF;
    Hashtbl.add shapeEnvCache fnId shapeEnv;   
    shapeEnv 
    
and infer_normalized_shape_env (fnTable : FnTable.t) (fundef : SSA.fundef) = 
  let fnId = fundef.SSA.fn_id in 
  try Hashtbl.find normalizedShapeEnvCache fnId 
  with _ ->  begin
    let rawShapeEnv = infer_shape_env fnTable fundef in
    let inputIdSet = ID.Set.of_list fundef.SSA.input_ids in
    let normalizer id shape normalizedEnv =
      (* if already normalized, don't do it again *) 
      if ID.Map.mem id normalizedEnv then normalizedEnv
      else    
        let shape', normalizedEnv' =
          normalize_shape inputIdSet rawShapeEnv normalizedEnv shape 
        in
        ID.Map.add id shape' normalizedEnv' 
    in   
    let normalizedEnv = ID.Map.fold normalizer rawShapeEnv ID.Map.empty in  
    Hashtbl.add normalizedShapeEnvCache fnId normalizedEnv;
    normalizedEnv 
 end   

and infer_normalized_output_shapes (fnTable : FnTable.t) (fundef : SSA.fundef) = 
  let fnId = fundef.SSA.fn_id in 
  try Hashtbl.find normalizedOutputShapeCache fnId 
  with _ -> begin 
    let shapeEnv = infer_shape_env fnTable fundef in
    let inputSet = ID.Set.of_list fundef.input_ids in
    let rawShapes = 
      List.map (fun id -> ID.Map.find id shapeEnv) fundef.output_ids
    in
    let normalizedShapes, _ = 
      normalize_shape_list inputSet shapeEnv ID.Map.empty rawShapes
    in 
    Hashtbl.add normalizedOutputShapeCache fnId normalizedShapes;
    normalizedShapes    
 end

and infer_call_result_shapes fnTable fundef argShapes = 
  let normalizedOutputShapes = infer_normalized_output_shapes fnTable fundef in   
  (* once the shape expressions only refer to input IDs, 
     remap those input IDs argument expressions 
   *)  
  let argEnv : shape ID.Map.t = 
    List.fold_left2 
      (fun env id argShape -> ID.Map.add id argShape env)
      ID.Map.empty 
      fundef.SSA.input_ids 
      argShapes
  in 
  let resultShapes = 
    List.map (SymbolicShape.rewrite_shape argEnv) normalizedOutputShapes
  in 
  IFDEF DEBUG THEN 
    Printf.printf "[ShapeInference] Calling %s with %s => %s\n"
      (FnId.to_str fundef.SSA.fn_id)
      (SymbolicShape.shapes_to_str argShapes)
      (SymbolicShape.shapes_to_str resultShapes);
  ENDIF; 
  resultShapes 
       
   