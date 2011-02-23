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
    
    let phi env leftEnv rightEnv phiNode =
      let leftShape = value leftEnv phiNode.phi_left in 
      let rightShape = value rightEnv phiNode.phi_right in 
      if leftShape <> rightShape then failwith "Shape error";
      let id = phiNode.phi_id in 
      if ID.Map.mem id env then ( 
        let oldShape = ID.Map.find id env in 
        if leftShape <> oldShape then failwith "Shape error"
        else None 
      )
      else Some (ID.Map.add id leftShape env)
   
    
    let exp env expNode helpers = match expNode.exp with
      | SSA.Call(fnId, args) -> 
          raise (ShapeInferenceFailure "unexpected function call")
      | SSA.PrimApp (Prim.ArrayOp Prim.Index, array::indices) -> 
        let arrayShape =  value env array in
        let nIndices = List.length indices in
        (* for now assume slicing can only happen along the 
           outermost dimensions and only by scalar indices 
         *)  
        IFDEF DEBUG THEN assert (List.length arrayShape > nIndices); ENDIF;
        [List.drop nIndices arrayShape]
      | SSA.PrimApp (Prim.ArrayOp Prim.Where, [array]) ->
        (* an upper bound would be: [value env array], but 
           for now fail if we can't be exact 
         *)
         raise (ShapeInferenceFailure "Can't infer output shape of WHERE prim")
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
          let argShapes = List.map (value env) args in
          (* TODO: make this efficient-- don't need so many list traversals *) 
          let ranks = List.map rank argShapes in 
          let maxRank = List.fold_left max 0 ranks in 
          let eltShapes = 
            List.map 
              (fun shape -> 
                if rank shape = maxRank then peel_shape shape else shape)
              argShapes 
          in
          let eltInShapes = (closArgShapes @ eltShapes) in 
          let eltOutShapes = P.output_shapes closure.closure_fn eltInShapes in  
          (* collect only args of maximal rank *) 
          let maxVecShapes =
            List.filter (fun shape -> rank shape = maxRank) argShapes 
          in 
          (* combine first dim of each maximal rank into single exp *) 
          let maxDim : Imp.exp_node = 
            mk_max_dim (List.map List.hd maxVecShapes) 
          in
          let vecOutShapes = 
            List.map (fun outShape -> maxDim :: outShape) eltOutShapes
          in  
          IFDEF DEBUG THEN 
            Printf.printf "\t MAP (%s)(%s)\n"
              (SSA.closure_to_str closure)
              (SSA.value_nodes_to_str args)
            ; 
            Printf.printf "\t\t ranks: %s\n" 
              (String.concat ", " (List.map string_of_int ranks))
            ;
            Printf.printf "\t\t fn input shapes: %s\n" 
              (shapes_to_str eltInShapes)
            ; 
            Printf.printf "\t\t fn output shapes: %s\n" 
              (shapes_to_str eltOutShapes)
            ; 
            Printf.printf "\t\t maxVecShapes: %s\n" 
              (shapes_to_str maxVecShapes)
            ; 
            Printf.printf "\t\t maxDim: %s\n" (Imp.exp_node_to_str maxDim);
            Printf.printf "\t\t final output shapes: %s\n"
              (shapes_to_str vecOutShapes); 
          ENDIF;  
          vecOutShapes
          
          
      | SSA.Reduce(initClos, _, initArgs, args) -> 
          let initClosArgShapes : shape list = 
            List.map (value env) initClos.closure_args in
          let initShapes : shape list = List.map (value env) initArgs in 
          let argShapes : shape list = List.map (value env) args in
          let ranks = List.map rank argShapes in
          let maxRank = List.fold_left max 0 ranks in
          let eltShapes = 
            List.map 
              (fun shape -> 
                if rank shape = maxRank then peel_shape shape else shape)
              argShapes 
          in
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
   intermediate variables, "canonicalize" it to only refer to input variables
*)   
let rec canonicalize_dim inputSet rawShapeEnv canonicalEnv expNode
        : Imp.exp_node * shape ID.Map.t  = 
  match expNode.Imp.exp with  
  | Imp.Op (op, t, args) ->
      (* a list of dims and a shape are the same thing, so just reuse the
         canonicalize_shape function for a list of dim arguments 
      *) 
      let args', canonicalEnv' = 
        canonicalize_shape inputSet rawShapeEnv canonicalEnv args 
      in 
      let expNode' = {expNode with Imp.exp = Imp.Op(op, t, args') } in
      ImpSimplify.simplify_arith expNode', canonicalEnv'   
  | Imp.Const n -> expNode, canonicalEnv   
  | Imp.DimSize (d, {Imp.exp=Imp.Var id}) ->
      if ID.Set.mem id inputSet then expNode, canonicalEnv 
      else 
        let shape, canonicalEnv' = 
          if ID.Map.mem id canonicalEnv then 
            ID.Map.find id canonicalEnv, canonicalEnv  
          else   
            (* if some local variable's shape has not yet been canonicalized,
               do so recursively 
            *) 
            let rawShape = ID.Map.find id rawShapeEnv in 
            let canonicalShape, canonicalEnv' = 
              canonicalize_shape inputSet rawShapeEnv canonicalEnv rawShape 
            in
            canonicalShape, ID.Map.add id canonicalShape canonicalEnv'
        in  
        (if List.length shape < d then failwith "Shape of insufficient rank");   
        List.nth shape d, canonicalEnv'   
  | Imp.Var id -> failwith "variables should only appear in dimsize expressions"
  | _ ->failwith ("unexpected Imp expression: " ^ (Imp.exp_node_to_str expNode))
  
and canonicalize_shape inputSet rawShapeEnv canonicalEnv shape 
    : shape * shape ID.Map.t  =
  let foldFn (revDims, canonicalEnv) currDim = 
    let currDim', canonicalEnv' = 
      canonicalize_dim inputSet rawShapeEnv canonicalEnv currDim 
    in  
    currDim' :: revDims, canonicalEnv' 
  in 
  
  let revShape, env' = List.fold_left foldFn ([], canonicalEnv) shape in
  List.rev revShape, env' 

let rec canonicalize_shape_list inputSet rawShapeEnv canonicalEnv = function 
  | [] -> [], canonicalEnv 
  | shape::rest -> 
      let rest', canonicalEnv' = 
        canonicalize_shape_list inputSet rawShapeEnv canonicalEnv rest 
      in
      let shape', canonicalEnv'' =
        canonicalize_shape inputSet rawShapeEnv canonicalEnv shape
      in 
      shape'::rest', canonicalEnv''     


(* cache the canonical output shape expressions of each function. "Canonical" 
   means the expressions refer to input IDs, which need to be replaced by some 
   input expression later
*)
let canonicalOutputShapeCache : (FnId.t, shape list) Hashtbl.t = 
  Hashtbl.create 127
  
let rec infer_shape_env (fnTable:FnTable.t) (fundef : SSA.fundef)   =  
  let module Params : PARAMS = struct 
    let output_shapes fnId argShapes =
      let fundef = FnTable.find fnId fnTable in 
      let canonicalOutputShapes = infer_output_shapes fnTable fundef in   
      (* once the shape expressions only refer to input IDs, 
         remap those input IDs argument expressions *)  
      let argEnv : shape ID.Map.t = 
        List.fold_left2 
              (fun env id argShape -> ID.Map.add id argShape env)
              ID.Map.empty 
              fundef.SSA.input_ids 
              argShapes
      in 
      List.map (SymbolicShape.rewrite_shape argEnv) canonicalOutputShapes
  end 
  in 
  let module ShapeEval = SSA_Analysis.MkEvaluator(ShapeAnalysis(Params)) in 
  let shapeEnv = ShapeEval.eval_fundef fundef in 
  IFDEF DEBUG THEN
    Printf.printf "Inferred shape environment for %s : %s -> %s:\n"
      (FnId.to_str fundef.SSA.fn_id)
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
  shapeEnv 

and infer_output_shapes (fnTable : FnTable.t) (fundef : SSA.fundef) = 
  let fnId = fundef.SSA.fn_id in 
  try Hashtbl.find canonicalOutputShapeCache fnId 
  with _ -> 
    let shapeEnv = infer_shape_env fnTable fundef in
    let inputSet = ID.Set.of_list fundef.input_ids in
    let rawShapes = 
      List.map (fun id -> ID.Map.find id shapeEnv) fundef.output_ids
    in
    let canonicalShapes, _ = 
      canonicalize_shape_list inputSet shapeEnv ID.Map.empty rawShapes
    in 
    Hashtbl.add canonicalOutputShapeCache fnId canonicalShapes; 
    canonicalShapes    