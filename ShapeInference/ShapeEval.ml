(* pp: -parser o pa_macro.cmo *)

open Base
open Imp 

module type MATHOPS = sig 
  type t 
  val safe_div : t -> t -> t 
  val log : t -> t
  val mul : t -> t -> t 
  val add : t -> t -> t
  val of_int : int -> t
  val of_int32 : Int32.t -> t   
  val of_float : float -> t 
end

module IntOps = struct 
  type t = int 
  let safe_div = safe_div 
  let log x =  int_of_float (ceil (log (float_of_int x)))
  let mul x y = x * y
  let add x y = x + y
  let of_int x = x
  let of_int32 x = Int32.to_int x 
  let of_float x = int_of_float x 
end 

module FloatOps = struct 
  type t = int 
  let safe_div = safe_div 
  let log x =  int_of_float (ceil (log (float_of_int x)))
  let mul x y = x * y
  let add x y = x + y
  let of_int x = x
  let of_int32 x = Int32.to_int x 
  let of_float x = int_of_float x 
end 

module MkExpEval (M : MATHOPS)  = struct 
    let rec eval_exp (shapeEnv:Shape.t ID.Map.t) expNode : M.t = 
      match expNode.exp with  
      | Op (Prim.Add, _, [arg1; arg2]) -> 
        M.add (eval_exp shapeEnv arg1) (eval_exp shapeEnv arg2)   
      | Op(Prim.Add, _, _) -> 
        failwith "[ShapeEval] wrong number of args for addition"
      | Op(Prim.SafeDiv, _, [arg1; arg2]) -> 
        M.safe_div (eval_exp shapeEnv arg1) (eval_exp shapeEnv arg2) 
      | Op (Prim.Mult, _, [arg1; arg2]) ->
        M.mul (eval_exp shapeEnv arg1) (eval_exp shapeEnv arg2) 
      | Op (Prim.Mult, _, _) ->
        failwith "[ShapeEval] wrong number of args for multiplication"
      | Op(Prim.Log, _, [arg]) -> M.log (eval_exp shapeEnv arg) 
      | Const (PQNum.Int16 i) -> M.of_int i
      | Const (PQNum.Int32 i32) -> M.of_int32 i32
      | Const n -> M.of_float (PQNum.to_float n)
      | DimSize(dim, {exp=Var id}) -> 
        if ID.Map.mem id shapeEnv then 
          let shape = ID.Map.find id shapeEnv in 
          M.of_int (Shape.get shape dim)
        else failwith $ Printf.sprintf 
          "[ShapeEval] shape not found for %s" (ID.to_str id)    
      | other -> failwith $ 
         Printf.sprintf "[ShapeEval] Unexpected expression: %s"
           (Imp.exp_to_str other)
end 

let eval_exp_as_int = MkExpEval(IntOps).eval_exp
let eval_exp_as_float = MkExpEval(FloatOps).eval_exp  
  
(*module SymbolicExpEval = MkExpEval(SymbolicOps)*)


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
