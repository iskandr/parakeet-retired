open Base
open SSA

(* describes the data which needs to be allocated before 
   calling a function with vectors as outputs or local temporaries 
*) 
type required_allocs = { 
  output_shapes : Shape.t list;
  output_types : DynType.t list;
  (* these are purely positional-- how do we ensure that the same
     order is preserved?  
  *)    
  local_shapes : Shape.t list;
  local_types : DynType.t list;
}


(* returns a list of ID.t, Shape.t pairs for all non-scalar locals *)
let gather_vector_locals shapeEnv inputIds outputIds = 
  ID.Map.fold  
      (fun id shape accList -> 
          if not $ (List.mem id inputIds || List.mem id outputIds)
          then (id, shape)::accList
          else accList
       )
      shapeEnv 
      []
  

(* shapes of values returned by a function  *)
let rec infer_call fnTable env fundef args = 
  let argShapes = List.map (infer_value env) args in 
  let allocs = infer_fundef fnTable fundef argShapes in
  allocs.output_shapes   
  

and infer_value (env : Shape.t ID.Map.t) (vNode : SSA.value_node) : Shape.t = 
  match vNode.value with 
  | Var id -> ID.Map.find id env 
  | Num _  
  | Str _  
  | Sym _ 
  | Unit -> Shape.scalar_shape
  | Prim _
  | Lam _
  | GlobalFn _ -> failwith "[ShapeInference] functions have no shape"
  
  
and infer_exp 
      (fnTable : FnTable.t) 
      (env : Shape.t ID.Map.t) 
      (expNode : SSA.exp_node) : Shape.t list =
  match expNode.exp with  
  | App ({value=Lam fundef}, args) -> infer_call fnTable env fundef args 
  | App ({value=GlobalFn fnId}, args) -> 
      let fundef = FnTable.find fnId fnTable in
      infer_call fnTable env fundef args   
  | App ({value=Prim (Prim.ArrayOp op)}, fnVal::args) when Prim.is_adverb op ->
      let fundef = FnTable.get_fundef fnTable fnVal in
      let argShapes = List.map (infer_value env) args in   
      let allocs = infer_adverb fnTable op fundef argShapes in 
      allocs.output_shapes
  | ArrayIndex (array, indices) -> 
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
      ID.Map.extend env ids (infer_exp fnTable env rhs)  
  | Ignore exp -> env (* assume this can't change any shapes *)  
  | SetIdx(id, indices, rhs) -> env 
      (* assume we can't change the shape of an array with 
         index assignment. 
      *)
  | If (condVal, tBlock, fBlock, ifGate) -> env  
      
  (*| WhileLoop (condExp, body, loopGate) ->*)   
and infer_body (fnTable : FnTable.t) (env : Shape.t ID.Map.t) = function 
  | [] -> env 
  | stmtNode::rest -> 
      let env' = infer_stmt fnTable env stmtNode in 
      infer_body fnTable env' rest  

and infer_fundef fnTable fundef inputShapes = 
  let env =
    ID.Map.extend ID.Map.empty fundef.input_ids inputShapes  
  in 
  let env' = infer_body fnTable env fundef.body in
  let vectorLocals = 
     gather_vector_locals env' fundef.input_ids fundef.output_ids
  in
  {
    output_shapes = 
      List.map (fun id -> ID.Map.find id env') fundef.output_ids; 
    output_types = 
      List.map (fun id -> ID.Map.find id fundef.tenv) fundef.output_ids;   
    local_shapes = List.map (fun (_, shape) -> shape) vectorLocals; 
    local_types = 
      List.map (fun  (id, _) -> ID.Map.find id fundef.tenv) vectorLocals;
  }  

and infer_adverb fnTable op fundef argShapes = match op with 
  | Prim.Map -> infer_map fnTable fundef argShapes 
  | Prim.AllPairs -> infer_allpairs fnTable fundef argShapes
  | Prim.Reduce -> infer_reduce fnTable fundef argShapes 
  | _ -> failwith "[ShapeInference] adverb not implemented"

and infer_map fnTable fundef argShapes = 
  match Shape.max_shape_list argShapes with
    | None -> failwith "incompatible shapes passed to Map"
    | Some maxShape -> 
      assert (Shape.rank maxShape > 0);  
      let outerDim = Shape.get maxShape 0 in 
      let nestedInputShapes = List.map Shape.peel_shape argShapes in 
      let nestedAllocs = infer_fundef fnTable fundef nestedInputShapes in
      { 
        output_types = 
          List.map (fun t -> DynType.VecT t) nestedAllocs.output_types;
        output_shapes = 
          List.map 
            (fun s -> Shape.append_dim outerDim s) 
            nestedAllocs.output_shapes;
        (* these are interpreted as the local types per function evaluation *)
        local_types = 
          List.map (fun t -> DynType.VecT t) nestedAllocs.local_types;
        local_shapes = 
          List.map (fun s -> Shape.append_dim outerDim s)
          nestedAllocs.local_shapes
      }   

and infer_reduce fnTable fundef argShapes = 
  let nestedInputShapes = List.map Shape.peel_shape argShapes in
  (* the outputs of the reduction function are also
       the outputs of the whole adverb 
  *) 
  infer_fundef fnTable fundef nestedInputShapes 
     
    
and infer_allpairs fnTable fundef argShapes = 
   match argShapes with  
    | [argShape1; argShape2] ->
      assert (Shape.rank argShape1 > 0);
      assert (Shape.rank argShape2 > 0); 
      let m = Shape.get argShape1 0 in 
      let n = Shape.get argShape2 0 in    
      let nestedAllocs = 
        infer_fundef fnTable fundef (List.map Shape.peel_shape argShapes)  
      in          
      { 
        output_types = 
          List.map 
            (fun t -> DynType.VecT (DynType.VecT t)) 
            nestedAllocs.output_types; 
        output_shapes = 
          List.map 
            (fun s -> Shape.append_dims [m; n] s)
            nestedAllocs.output_shapes; 
        local_types = nestedAllocs.local_types; 
        local_shapes = nestedAllocs.local_shapes;            
      }
        
    | _ -> failwith "expected two shapes for all-pairs operator"   
  
   