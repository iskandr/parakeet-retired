(* pp: -parser o pa_macro.cmo *)

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
type env = SymbolicShape.t ID.Map.t 
module type PARAMS = sig 
  val output_shapes : FnId.t -> SymbolicShape.t list -> SymbolicShape.t list  
end 

module ShapeAnalysis (P: PARAMS) =  struct
    type value_info = SymbolicShape.t 
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
          let rank = Type.rank (ID.Map.find id fundef.SSA.tenv) in 
          let shape = SymbolicShape.all_dims id rank in 
          ID.Map.add id shape accEnv
        )
        ID.Map.empty 
        fundef.input_ids 
   
    let value env valNode = match valNode.value with 
      | SSA.Var id -> SymbolicShape.all_dims id (Type.rank valNode.SSA.value_type)   
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
      | SSA.Call(fnId, args) -> P.output_shapes fnId (get_shapes args) 
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
          [SymbolicShape.concat idxShape (SymbolicShape.peel arrayShape)]
             
        )
      | SSA.PrimApp (Prim.ArrayOp Prim.Where, [array]) ->
        (* an upper bound would be: [value env array], but 
           for now fail if we can't be exact 
         *)
         let arrayShape = value env array in
         let n = SymbolicShape.prod_of_dims arrayShape in
         [[n]]
      | SSA.PrimApp (Prim.ArrayOp Prim.DimSize, [_; _]) 
      | SSA.PrimApp (Prim.ArrayOp Prim.Find, [_; _]) -> [SymbolicShape.scalar]
      | SSA.PrimApp (Prim.ScalarOp _, args) when 
        List.for_all (fun arg -> Type.is_scalar arg.value_type) args -> 
          [SymbolicShape.scalar]
      | SSA.Arr elts ->
        let eltShapes = List.map (value env) elts in
        (* TODO: check that elt shapes actually match each other *) 
        let n = List.length eltShapes in
        [SymbolicShape.Const n :: (List.hd eltShapes)]             
      | SSA.Cast (t, v) ->  [value env v] 
      | SSA.Values vs -> List.map (value env) vs  
      | SSA.Adverb (adverb, closure, args) -> 
          (* 
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
          *)
          assert false   
      | other -> 
          let expStr = SSA.exp_to_str expNode in 
          failwith (Printf.sprintf "[shape_infer] not implemented: %s\n" expStr) 
      
      let stmt env stmtNode helpers = match stmtNode.stmt with
      | Set(ids, rhs) ->
          let newShapes = exp env rhs helpers in
          IFDEF DEBUG THEN 
            if List.length ids <> List.length newShapes then 
              failwith ("Shape inference error in stmt '" ^
                        (SSA.stmt_node_to_str stmtNode) ^ 
                        "': number of IDs must match number of rhs shapes");
          ENDIF;
          let prevDefined = List.for_all (fun id ->ID.Map.mem id env) ids in 
          let changed = 
            not prevDefined ||
            let oldShapes = List.map (fun id -> ID.Map.find id env) ids in 
            List.exists2 (<>) oldShapes newShapes
          in
          if not changed then None
          else 
          let env' =
            List.fold_left2 
              (fun env id shape -> ID.Map.add id shape env)
              env ids newShapes 
          in Some env'   
      | _ -> helpers.eval_stmt env stmtNode  
   
end


(* given a dim expression (one elt of a shape) which may reference 
   intermediate variables, "normalize" it to only refer to input variables
*)

let rec normalize_dim 
          (inputSet: ID.Set.t) 
          (rawShapeEnv: SymbolicShape.t ID.Map.t)
          (normalizedEnv: SymbolicShape.t ID.Map.t)  = function 
  | Dim (id, idx) when not (ID.Set.mem id inputSet) -> 
    if ID.Map.mem id normalizedEnv then
      let shape = ID.Map.find id normalizedEnv in 
      SymbolicShape.get_dim shape idx, normalizedEnv
    else 
      let rawShape = ID.Map.find id rawShapeEnv in 
      let normalizedShape, normalizedEnv' = 
        normalize_shape inputSet rawShapeEnv normalizedEnv rawShape
      in 
      let normalizedEnv'' = ID.Map.add id normalizedShape normalizedEnv' in 
      SymbolicShape.get_dim normalizedShape idx, normalizedEnv''
  | Op (op, x, y) -> 
    let x', normalizedEnv' = normalize_dim inputSet rawShapeEnv normalizedEnv x in
    let y', normalizedEnv'' = normalize_dim inputSet rawShapeEnv normalizedEnv' y in 
    let resultDim = SymbolicShape.simplify_op op x' y' in 
    resultDim, normalizedEnv''
  | other -> other, normalizedEnv 
and normalize_shape inputSet rawShapeEnv normalizedEnv shape 
    : SymbolicShape.t * SymbolicShape.t ID.Map.t  =
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
let normalizedOutputShapeCache : (FnId.t, SymbolicShape.t list) Hashtbl.t = 
  Hashtbl.create 127
  
let shapeEnvCache : (FnId.t, SymbolicShape.env) Hashtbl.t = Hashtbl.create 127
(* like the shape env cache, except the shape expressions have all been 
   normalize to refer only to input variables and not intermediates
*) 
let normalizedShapeEnvCache : (FnId.t, SymbolicShape.env) Hashtbl.t = 
  Hashtbl.create 127 
  
  
let rec infer_shape_env (fnTable:FnTable.t) (fundef : SSA.fn) =
  let fnId = fundef.SSA.fn_id in
  (*
  IFDEF DEBUG THEN 
    Printf.printf 
      "[ShapeInference::infer_shape_env] Looking up shape env for %s\n" 
      (FnId.to_str fnId);
  ENDIF;
  *)  
  try Hashtbl.find shapeEnvCache fnId  
  with _ -> 
    let module Params : PARAMS = struct 
      let output_shapes fnId argShapes =
        let fundef = FnTable.find fnId fnTable in 
        infer_call_result_shapes fnTable fundef argShapes 
    end 
    in 
    let module ShapeEval = SSA_Analysis.MkEvaluator(ShapeAnalysis(Params)) in 
    let shapeEnv = ShapeEval.eval_fn fundef in
    Hashtbl.add shapeEnvCache fnId shapeEnv;   
    shapeEnv 
    
and infer_normalized_shape_env (fnTable : FnTable.t) (fundef : SSA.fn) = 
  let fnId = fundef.SSA.fn_id in
  try 
    Hashtbl.find normalizedShapeEnvCache fnId 
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

and infer_normalized_output_shapes (fnTable : FnTable.t) (fundef : SSA.fn) = 
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
  let argEnv : SymbolicShape.t ID.Map.t = 
    List.fold_left2 
      (fun env id argShape -> ID.Map.add id argShape env)
      ID.Map.empty 
      fundef.SSA.input_ids 
      argShapes
  in 
  let resultShapes = 
    List.map (SymbolicShape.rewrite_shape argEnv) normalizedOutputShapes
  in 
  resultShapes 
       
   
