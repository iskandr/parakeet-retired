open Base
open Imp 

let rec eval_size_expression shapeEnv exp = 
  (*
  IFDEF DEBUG THEN 
    Printf.printf "[ImpShapeInference] evaluating size expression: %s\n"
    (Imp.exp_to_str exp);
  ENDIF;
  *) 
  match exp with  
  | Op (Prim.Add, _, [arg1; arg2]) -> 
      let x1 = eval_size_expression shapeEnv arg1.exp in 
      let x2 = eval_size_expression shapeEnv arg2.exp in 
      x1 + x2   
  | Op(Prim.Add, _, _) -> 
      failwith "[ImpShapeInference] wrong number of args for addition"
  | Op(Prim.SafeDiv, _, [arg1; arg2]) -> 
      let x1 = eval_size_expression shapeEnv arg1.exp in 
      let x2 = eval_size_expression shapeEnv arg2.exp in 
      safe_div x1 x2 
  | Op (Prim.Mult, _, [arg1; arg2]) ->
      let x1 = eval_size_expression shapeEnv arg1.exp in 
      let x2 = eval_size_expression shapeEnv arg2.exp in 
      x1 * x2 
  | Op (Prim.Mult, _, _) ->
      failwith "[ImpShapeInference] wrong number of args for multiplication"
  
  | Const n -> PQNum.to_int n
  | DimSize(dim, {exp=Var id}) -> 
      if ID.Map.mem id shapeEnv then 
        let shape = ID.Map.find id shapeEnv in 
        Shape.get shape dim
      else failwith $ Printf.sprintf 
        "[ImpShapeInference] shape not found for %s" (ID.to_str id)    
  | other -> failwith $ 
      Printf.sprintf "[ShapeInference] Unexpected expression: %s"
      (Imp.exp_to_str other)


let infer_shapes fn (inputShapes : Shape.t list) =
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
    let dims = List.map (eval_size_expression shapeEnv) sizeExpressions in
     
    let shape = Shape.of_list dims in   
    IFDEF DEBUG THEN 
      Printf.printf "Inferred shape for %s: %s  \n"
        (ID.to_str id)
        (Shape.to_str shape)
      ;
    ENDIF; 
    ID.Map.add id shape shapeEnv     
  in 
  (* shapes of inputs and outputs *) 
  let outputEnv = 
    PMap.foldi aux fn.output_sizes inputEnv 
  in 
  let aux2 id annot shapeEnv = match annot with 
    | Imp.PrivateArray sizes -> aux id sizes shapeEnv
    | Imp.SharedArray _ -> shapeEnv  
  in         
  PMap.foldi aux2 fn.local_arrays  outputEnv  

  