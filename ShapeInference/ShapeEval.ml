(* pp: -parser o pa_macro.cmo *)

open Base
open Imp 
open MathEval 


let eval_dim_as_int shapeEnv = function 
  | Const i -> i  
  | Dim (id, idx) -> 
      if ID.Map.mem id shapeEnv then 
        SymbolicShape.get_dim (ID.Map.find id shapeEnv) idx
      else 
        failwith $ Printf.sprintf  
          "[ShapeEval] shape not found for %s" (ID.to_str id)   
  | Op (op, x, y) ->
      let fn = match op with 
        | Add -> (+)
        | Mult ->  ( * ) 
        | Max -> max 
      in fn (eval_dim_as_int shapeEnv x) (eval_dim_as_int shapeEnv y) 
       
let eval_dim_as_float shapeEnv = function 
  | Const i -> float_of_int i 
  | Dim (id, idx) -> 
      if ID.Map.mem id shapeEnv then
        let shape = ID.Map.find id shapeEnv in 
        float_of_int (SymbolicShape.get_dim shape idx)
      else 
        failwith $ Printf.sprintf  
          "[ShapeEval] shape not found for %s" (ID.to_str id)   
  | Op (op, x, y) ->
      let fn = match op with 
        | Add -> (+.)
        | Mult ->  ( *. ) 
        | Max -> max 
      in fn (eval_dim_as_float shapeEnv x) (eval_dim_as_float shapeEnv y) 
       

let eval_shape shapeEnv (symShape : SymbolicShape.t) : Shape.t = 
  Shape.of_list (List.map (eval_dim_as_int shapeEnv) symShape)
  
let eval_imp_shape_env (fn:Imp.fn) (inputShapes : Shape.t list) =
  (* shapes of all inputs *) 
  let inputEnv = 
    List.fold_left2  
      (fun accEnv id shape -> ID.Map.add id shape accEnv)
      ID.Map.empty
      fn.input_ids
      inputShapes
  in
  let aux id symShape shapeEnv  =
    let shape = eval_shape shapeEnv symShape in 
    ID.Map.add id shape shapeEnv     
  in
  ID.Map.fold aux fn.shapes inputEnv 

let eval_ssa_output_shapes 
      (fnTable:FnTable.t) 
      (fundef:SSA.fundef) 
      (inputShapes : Shape.t list) : Shape.t list = 
  let symbolicOutputShapes = 
    ShapeInference.infer_normalized_output_shapes fnTable fundef 
  in 
  let shapeEnv = ID.Map.extend ID.Map.empty fundef.SSA.input_ids inputShapes in 
  List.map (eval_shape shapeEnv) symbolicOutputShapes 
  
let eval_ssa_shape_env 
      (fnTable: FnTable.t) 
      (fundef:SSA.fundef) 
      (inputShapes : Shape.t list) : Shape.t ID.Map.t =
  (* infer symbolic shapes associated with each variable in a function *) 
  let symShapes = ShapeInference.infer_normalized_shape_env fnTable fundef in
  (* concrete shapes of the arguments *) 
  let initShapeEnv = 
    ID.Map.extend ID.Map.empty fundef.SSA.input_ids inputShapes 
  in
  (* evaluate symbolic shapes to get concrete shapes *)  
  ID.Map.map (eval_shape initShapeEnv) symShapes 



let get_call_output_shapes fn (inputs : shape list) =  
  let replaceMap = 
    ID.Map.extend ID.Map.empty (Array.to_list fn.input_ids) inputs 
  in
  let rawOutputShapes = 
    List.map (fun id -> ID.Map.find id fn.shapes) fn.output_ids
  in
  rewrite_shapes replaceMap rawOutputShapes
                
