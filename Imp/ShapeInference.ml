(* pp: -parser o pa_macro.cmo *)

open Base 
open Base
open SSA
open SSA_Analysis 

(* takes an SSA function and a map of its IDs to their counterparts in some
   future Imp function. Infers an Imp.exp array corresponding to each
   variable's shape 
*)  

type shape = Imp.exp_node list

let rec simplify_dim expNode = match expNode.Imp.exp with
  | Imp.Op (Prim.Add, t, [{Imp.exp = Imp.Const n}; x])
  | Imp.Op (Prim.Add, t, [x; {Imp.exp = Imp.Const n}])
    when PQNum.is_zero n ->  Imp.cast t x
  | Imp.Op (Prim.Mult, t, [{Imp.exp = Imp.Const n}; x])
  | Imp.Op (Prim.Mult, t, [x; {Imp.exp = Imp.Const n}])
    when PQNum.is_zero n -> 
    Imp.cast t {expNode with Imp.exp = Imp.Const n}
    
  | Imp.Op(op, t, [{Imp.exp = Imp.Const n1}; {Imp.exp = Imp.Const n2}])
      when DynType.is_integer t &&
           DynType.sizeof t < DynType.sizeof DynType.Int32T -> 
      let opFn = match op with 
      | Prim.Add -> (+) | Prim.Sub -> (-)  
      | Prim.Mult -> ( * ) | Prim.Div -> (/) 
      | Prim.Min -> min  | Prim.Max -> max
      | Prim.Mod -> (mod) 
      | _ -> 
        failwith ("op not implemented: " ^ (Prim.scalar_op_to_str op))
      in  
      let n3 = opFn (PQNum.to_int n1) (PQNum.to_int n2) in 
      { expNode with Imp.exp = Imp.Const (PQNum.coerce_int n3 t) }
  | Imp.Op(op, t, [{Imp.exp = Imp.Const n1}; {Imp.exp = Imp.Const n2}])
      when t = DynType.Int32T ->  
       
      let opFn = match op with 
      | Prim.Add -> Int32.add | Prim.Sub -> Int32.sub  
      | Prim.Mult -> Int32.mul | Prim.Mult -> Int32.mul 
      | Prim.Min ->  min | Prim.Max -> max
      | Prim.Mod -> Int32.rem
      | _ -> 
        failwith ("op not implemented: " ^ (Prim.scalar_op_to_str op))
      in  
      let n3 = opFn (PQNum.to_int32 n1) (PQNum.to_int32 n2) in 
      { expNode with Imp.exp = Imp.Const (PQNum.coerce_int32 n3 t) }
  | _ -> expNode 


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
      simplify_dim expNode', canonicalEnv'   
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

module type PARAMS = sig 
  val output_shapes : FnId.t -> shape list  
end 

module ShapeAnalysis (P: PARAMS) =  struct
(*
  let get_output_shapes f _ = [] 

  (* shapes of values returned by a function  *)
  let rec infer_call_outputs fnTable env fundef args : Shape.t list  = 
    let argShapes = List.map (infer_value env) args in 
    get_output_shapes fundef (infer_fundef fnTable fundef argShapes)
        

  and infer_value (env : Shape.t ID.Map.t) (vNode : SSA.value_node) : Shape.t = 
    match vNode.value with 
    | Var id -> ID.Map.find id env 
    | Num _  
    | Str _  
    | Sym _ 
    | Unit -> Shape.scalar_shape
    | Prim _
    | GlobalFn _ -> failwith "[ShapeInference] functions have no shape"
  
  
  and infer_exp 
      (fnTable : FnTable.t) 
      (env : Shape.t ID.Map.t) 
      (expNode : SSA.exp_node) : Shape.t list =
  match expNode.exp with  
  | App ({value=GlobalFn fnId}, args) -> 
      let fundef = FnTable.find fnId fnTable in
      infer_call_outputs fnTable env fundef args   
  | App ({value=Prim (Prim.ArrayOp Prim.Index)}, (array::indices)) -> 
      let arrayShape = infer_value env array in
      let nIndices = List.length indices in
      (* for now assume slicing can only happen along the 
         outermost dimensions and only by scalar indices 
      *)  
      let resultShape = ref arrayShape in 
      for i = 1 to nIndices do 
        resultShape := Shape.peel_shape !resultShape
      done; 
      [!resultShape] 
  | App ({value=Prim (Prim.Adverb op)}, fnVal::args) ->
      let fundef = FnTable.get_fundef fnTable fnVal in
      let argShapes = List.map (infer_value env) args in   
      let outputs, _ = infer_adverb fnTable op fundef argShapes in 
      outputs
  | App (_, args) when 
      List.for_all (fun arg -> DynType.is_scalar arg.value_type) args ->
      [Shape.scalar_shape]
  | App _ -> failwith "unsupported function type"
                
  | Arr elts ->
      let eltShapes = List.map (infer_value env) elts in
      begin match eltShapes with 
      | [] -> failwith "[ShapeInference] Cannot create empty array"
      | firstShape::rest ->
          if not $ List.for_all (fun shape -> Shape.eq shape firstShape) rest
          then 
            failwith "[ShapeInference] Array elements must have uniform shape"
          else 
            let n = List.length eltShapes in
            [Shape.append_dim n firstShape]  
      end            
  | Cast (t, v) ->  [infer_value env v] 
  | Values vs -> List.map (infer_value env) vs  
 
  and infer_stmt (fnTable : FnTable.t) (env : Shape.t ID.Map.t) stmtNode = 
    match stmtNode.stmt with  
    | Set (ids, rhs) ->  
      let rhsShapes = infer_exp fnTable env rhs in
      let nShapes = List.length rhsShapes in
      let nIds = List.length ids in   
      if nShapes <> nIds  then
        failwith $ Printf.sprintf 
          "[ShapeInference] statement \"%s\" expected %d inputs but received %d"
          (SSA.stmt_node_to_str stmtNode) nIds nShapes 
      else  
      ID.Map.extend env ids rhsShapes    
    | SetIdx(id, indices, rhs) -> env 
      (* assume we can't change the shape of an array with 
         index assignment. 
      *)
    | If (condVal, tBlock, fBlock, ifGate) -> env  
      
  (*| WhileLoop (condExp, body, loopGate) ->*)   
  and infer_body (fnTable : FnTable.t) (env : Shape.t ID.Map.t) block = 
    Block.fold_forward (infer_stmt fnTable) env block 
  and infer_fundef fnTable fundef inputShapes : shape_env = 
    let env =
      ID.Map.extend ID.Map.empty fundef.input_ids inputShapes  
    in 
    infer_body fnTable env fundef.body
  
  and infer_adverb fnTable op fundef argShapes = match op with 
  | Prim.Map -> infer_map fnTable fundef argShapes 
  | Prim.AllPairs -> infer_allpairs fnTable fundef argShapes
  | Prim.Reduce -> infer_reduce fnTable fundef argShapes 
  | _ -> failwith "[ShapeInference] adverb not implemented"

  and infer_map fnTable fundef argShapes : Shape.t list * shape_env = 
  match Shape.max_shape_list argShapes with
    | None -> failwith "incompatible shapes passed to Map"
    | Some maxShape -> 
      assert (Shape.rank maxShape > 0);  
      let outerDim = Shape.get maxShape 0 in 
      let nestedInputShapes = List.map Shape.peel_shape argShapes in 
      let nestedEnv = infer_fundef fnTable fundef nestedInputShapes in
      let outputShapes = 
        List.map 
          (fun s -> Shape.append_dim outerDim s) 
          (get_output_shapes fundef nestedEnv) 
      in 
      outputShapes, nestedEnv 
      
  and infer_reduce fnTable fundef argShapes : Shape.t list * shape_env  = 
  let nestedInputShapes = List.map Shape.peel_shape argShapes in
  let nestedEnv = infer_fundef fnTable fundef nestedInputShapes in
  (* the outputs of the reduction function are also
       the outputs of the whole adverb 
  *) 
  (get_output_shapes fundef nestedEnv), nestedEnv  
     
    
  and infer_allpairs fnTable fundef argShapes : Shape.t list * shape_env = 
   match argShapes with  
    | [argShape1; argShape2] ->
      IFDEF DEBUG THEN 
        assert (Shape.rank argShape1 > 0);
        assert (Shape.rank argShape2 > 0);
      ENDIF;  
      let m = Shape.get argShape1 0 in 
      let n = Shape.get argShape2 0 in    
      let nestedEnv = 
        infer_fundef fnTable fundef (List.map Shape.peel_shape argShapes)  
      in
      let outputShapes = 
        List.map 
          (fun s -> Shape.append_dims [m; n] s)
          (get_output_shapes fundef nestedEnv)
      in  
      outputShapes, nestedEnv 
    | _ -> failwith "expected two shapes for all-pairs operator"   
   *)
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
          ID.Map.add id (Imp.all_dims varNode)  accEnv
        )
        ID.Map.empty 
        fundef.input_ids 
   
    let value env valNode = match valNode.value with 
      | SSA.Var id -> Imp.all_dims (Imp.var ~t:valNode.value_type id)  
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
         (*let fundef = FnTable.find fnId fnTable in
         helpers.eval_fundef 
         infer_call_outputs fnTable env fundef args*)
        assert false
      | SSA.PrimApp (Prim.ArrayOp Prim.Index, array::indices) -> 
        let arrayShape =  value env array in
        let nIndices = List.length indices in
        (* for now assume slicing can only happen along the 
           outermost dimensions and only by scalar indices 
         *)  
        IFDEF DEBUG THEN assert (List.length arrayShape > nIndices); ENDIF;
        [List.drop nIndices arrayShape]
      | SSA.PrimApp (Prim.ScalarOp _, args) when 
        List.for_all (fun arg -> DynType.is_scalar arg.value_type) args -> [[]]
     | Arr elts ->
        let eltShapes = List.map (value env) elts in
        (* TODO: check that elt shapes actually match each other *) 
        let n = List.length eltShapes in
        [Imp.int n :: (List.hd eltShapes)]             
      | Cast (t, v) ->  [value env v] 
      | Values vs -> List.map (value env) vs  
      | Map(closure, args) -> 
          let closArgShapes : shape list = 
            List.map (value env) closure.closure_args 
          in 
          let argShapes = List.map (value env) args in
          let fnOutputShapes = P.output_shapes closure.closure_fn in
          (* the output shape expressions reference input variables of the
             function, which need to replaced with the input values 
           *)  
          fnOutputShapes
          (* todo: replace input ids in fnOutputShapes with shapes of args *) 
           
      | other -> 
          let expStr = SSA.exp_to_str expNode in 
          failwith (Printf.sprintf "[shape_infer] not implemented: %s\n" expStr) 


    let stmt env stmtNode helpers = match stmtNode.stmt with 
      | Set(ids, rhs) ->
          let rhsVal = (exp env rhs helpers) in 
          let env' =
            List.fold_left 
              (fun env id -> ID.Map.add id [] env)
              env
              ids 
          in Some env' 
      | _ -> None    
end

        
let rec shape_infer (fnTable:FnTable.t) (fundef : SSA.fundef)  =
  let module Params = 
    struct 
      let output_shapes fnId = 
        let fundef = FnTable.find fnId fnTable in 
        let shapeEnv = shape_infer fnTable fundef in
        let inputSet = ID.Set.of_list fundef.input_ids in
        let rawShapes = 
          List.map (fun id -> ID.Map.find id shapeEnv) fundef.output_ids
        in   
        let canonicalShapes, _ = 
          canonicalize_shape_list 
            inputSet 
            shapeEnv
            ID.Map.empty
            rawShapes  
        in 
        canonicalShapes 
    end 
  in 
  let module ShapeEval = SSA_Analysis.MkEvaluator(ShapeAnalysis(Params)) in 
  ShapeEval.eval_fundef fundef 
      

