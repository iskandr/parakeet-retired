(* pp: -parser o pa_macro.cmo *)

open Base
open Imp 

let rec eval_dim shapeEnv expNode : int = 
  match expNode.exp with  
  | Op (Prim.Add, _, [arg1; arg2]) -> 
      let x1 = eval_dim shapeEnv arg1 in 
      let x2 = eval_dim shapeEnv arg2 in 
      x1 + x2   
  | Op(Prim.Add, _, _) -> 
      failwith "[ShapeEval] wrong number of args for addition"
  | Op(Prim.SafeDiv, _, [arg1; arg2]) -> 
      let x1 = eval_dim shapeEnv arg1 in 
      let x2 = eval_dim shapeEnv arg2 in 
      safe_div x1 x2 
  | Op (Prim.Mult, _, [arg1; arg2]) ->
      let x1 = eval_dim shapeEnv arg1 in 
      let x2 = eval_dim shapeEnv arg2 in 
      x1 * x2 
  | Op (Prim.Mult, _, _) ->
      failwith "[ShapeEval] wrong number of args for multiplication"
  
  | Const n -> PQNum.to_int n
  | DimSize(dim, {exp=Var id}) -> 
      if ID.Map.mem id shapeEnv then 
        let shape = ID.Map.find id shapeEnv in 
        Shape.get shape dim
      else failwith $ Printf.sprintf 
        "[ShapeEval] shape not found for %s" (ID.to_str id)    
  | other -> failwith $ 
      Printf.sprintf "[ShapeEval] Unexpected expression: %s"
      (Imp.exp_to_str other)

let eval_shape shapeEnv expNodeList : Shape.t = 
  Shape.of_list (List.map (eval_dim shapeEnv) expNodeList)
  
let eval_imp_shape_env (fn:Imp.fn) (inputShapes : Shape.t list) =
  IFDEF DEBUG THEN
    Printf.printf 
      "Inferring shapes for function (%s)->(%s) with input shapes %s\n"
      (String.concat ", " (Array.to_list (Array.map ID.to_str fn.input_ids)))
      (String.concat ", " (Array.to_list (Array.map ID.to_str fn.output_ids)))
      (String.concat ", " (List.map Shape.to_str inputShapes)) 
    ;
  ENDIF; 
  (* shapes of all inputs *) 
  let inputEnv = 
    Array.fold_left2 
    (fun accEnv id shape -> ID.Map.add id shape accEnv)
    ID.Map.empty
    fn.input_ids
    (Array.of_list inputShapes)
  in     
  let aux id sizeExpressions shapeEnv  =
    let dims = List.map (eval_dim shapeEnv) sizeExpressions in
    let shape = Shape.of_list dims in   
    IFDEF DEBUG THEN 
      Printf.printf "Inferred shape for %s: %s  \n"
        (ID.to_str id)
        (Shape.to_str shape);
    ENDIF; 
    ID.Map.add id shape shapeEnv     
  in 
  (* shapes of inputs and outputs *) 
  let outputEnv = Hashtbl.fold aux fn.output_sizes inputEnv in 
  let aux2 id annot shapeEnv = match annot with 
    | Imp.InputSlice sizes 
    | Imp.PrivateArray sizes -> aux id sizes shapeEnv
    | Imp.SharedArray _ -> shapeEnv  
  in         
  Hashtbl.fold aux2 fn.local_arrays  outputEnv  
  

let eval_ssa_output_shapes 
      (fnTable:FnTable.t) 
      (fundef:SSA.fundef) 
      (inputShapes : Shape.t list) : Shape.t list = 
  let symbolicOutputShapes = 
    ShapeInference.infer_output_shapes fnTable fundef 
  in 
  let shapeEnv = ID.Map.extend ID.Map.empty fundef.SSA.input_ids inputShapes in 
  List.map (eval_shape shapeEnv) symbolicOutputShapes 
