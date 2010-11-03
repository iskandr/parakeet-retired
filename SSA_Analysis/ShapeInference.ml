open Base
open SSA

type shape_env = Shape.t ID.Map.t 

let get_output_shapes (fundef : SSA.fundef) (env : shape_env) : Shape.t list = 
  List.map (fun id -> ID.Map.find id env) fundef.output_ids

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
  | Lam _
  | GlobalFn _ -> failwith "[ShapeInference] functions have no shape"
  
  
and infer_exp 
      (fnTable : FnTable.t) 
      (env : Shape.t ID.Map.t) 
      (expNode : SSA.exp_node) : Shape.t list =
  match expNode.exp with  
  | App ({value=Lam fundef}, args) -> 
      infer_call_outputs fnTable env fundef args 
  | App ({value=GlobalFn fnId}, args) -> 
      let fundef = FnTable.find fnId fnTable in
      infer_call_outputs fnTable env fundef args   
  | App ({value=Prim (Prim.ArrayOp op)}, fnVal::args) when Prim.is_adverb op ->
      let fundef = FnTable.get_fundef fnTable fnVal in
      let argShapes = List.map (infer_value env) args in   
      let outputs, _ = infer_adverb fnTable op fundef argShapes in 
      outputs
  | App (_, args) when 
      List.for_all (fun arg -> DynType.is_scalar arg.value_type) args ->
      [Shape.scalar_shape]
  | App _ -> failwith "unsupported function type"
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
      let rhsShapes = infer_exp fnTable env rhs in
      let nShapes = List.length rhsShapes in
      let nIds = List.length ids in   
      if nShapes <> nIds  then
        failwith $ Printf.sprintf 
          "[ShapeInference] statement \"%s\" expected %d inputs but received %d"
          (SSA.stmt_node_to_str stmtNode) nIds nShapes 
      else  
      ID.Map.extend env ids rhsShapes    
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
      assert (Shape.rank argShape1 > 0);
      assert (Shape.rank argShape2 > 0); 
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
  
   