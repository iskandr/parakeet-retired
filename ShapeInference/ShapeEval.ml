(* pp: -parser o pa_macro.cmo *)

open Base
open Imp 



type 'a math_ops = {   
  safe_div : 'a -> 'a -> 'a;  
  log : 'a -> 'a; 
  mul : 'a -> 'a -> 'a;  
  add : 'a -> 'a -> 'a; 
  of_int : int -> 'a; 
  of_pqnum : PQNum.num -> 'a; 
} 

let rec eval_exp (m : 'a math_ops) (shapeEnv:Shape.t ID.Map.t) expNode : 'a  =
  Printf.printf "[ShapeEval] eval_exp: %s\n"
    (Imp.exp_node_to_str expNode); 
  let recur (e : Imp.exp_node) : 'a  = eval_exp m shapeEnv e in   
  match expNode.exp with  
  | Op (Prim.Add, _, [arg1; arg2]) -> 
    m.add (recur arg1) (recur arg2)   
  | Op(Prim.Add, _, _) -> 
    failwith "[ShapeEval] wrong number of args for addition"
  | Op(Prim.SafeDiv, _, [arg1; arg2]) -> 
    m.safe_div (recur arg1) (recur arg2) 
  | Op (Prim.Mult, _, [arg1; arg2]) ->
    m.mul (recur arg1) (recur arg2) 
  | Op (Prim.Mult, _, _) ->
    failwith "[ShapeEval] wrong number of args for multiplication"
  | Op(Prim.Log, _, [arg]) -> m.log (recur arg) 
  | Const n -> m.of_pqnum n
  | DimSize(dim, {exp=Var id}) -> 
    if ID.Map.mem id shapeEnv then 
      let shape = ID.Map.find id shapeEnv in 
      m.of_int (Shape.get shape dim)
    else failwith $ Printf.sprintf  
      "[ShapeEval] shape not found for %s" (ID.to_str id)    
  | Cast(_, expNode') -> recur expNode'    
  | other -> failwith $ 
      Printf.sprintf "[ShapeEval] Unexpected expression: %s"
        (Imp.exp_to_str other)

let int_ops : int math_ops = {  
  safe_div = safe_div; 
  log =  (fun x -> int_of_float (ceil (log (float_of_int x))));
  mul = Int.mul;  
  add = ( + );
  of_int = (fun x -> x); 
  of_pqnum = PQNum.to_int;  
}



let float_ops : float math_ops = { 
  safe_div = (/.);  
  log = log; 
  mul = ( *. );
  add = ( +. ); 
  of_int = float_of_int; 
  of_pqnum = PQNum.to_float;   
} 

let eval_exp_as_int = (eval_exp int_ops)
let eval_exp_as_float = (eval_exp float_ops)   

let eval_shape shapeEnv expNodeList : Shape.t = 
  Shape.of_list (List.map (eval_exp_as_int shapeEnv) expNodeList)
  
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
    let dims = List.map (eval_exp_as_int shapeEnv) sizeExpressions in
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
    | _ -> assert false 
  in         
  Hashtbl.fold aux2 fn.local_arrays  outputEnv  
  

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
