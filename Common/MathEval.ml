open Base 

(* generic evaluator for scalar operations, used in both Eval and ShapeEval *)

type 'a math_ops = {   
  of_int : int -> 'a; 
  of_pqnum : PQNum.num -> 'a;
  to_str : 'a -> string; 
  
  safe_div : 'a -> 'a -> 'a;  
  log : 'a -> 'a; 
  mul : 'a -> 'a -> 'a;  
  add : 'a -> 'a -> 'a;    
} 


(* eval uniform math operators, where all args are the same type, 
   which is the same as the return type. This excludes predicates like
   less-than, etc... 
*) 
let eval (m : 'a math_ops) (op:Prim.scalar_op) (args : 'a list) = 
  match op, args with 
  | Prim.Max, [x;y] -> max x y 
  | Prim.Min, [x;y] -> min x y
  | Prim.Add, [x;y] -> m.add x y 
  | Prim.SafeDiv, [x;y] ->  m.safe_div x y  
  | Prim.Mult, [x;y] -> m.mul x y  
  | Prim.Log, [x]  -> m.log x
  | op, args -> failwith $ 
     Printf.sprintf "[MathEval] Operations %s not supported with arguments %s"
       (Prim.scalar_op_to_str op)
       (String.concat ", " (List.map m.to_str args)) 
  


let int_ops : int math_ops = {  
  safe_div = safe_div; 
  log =  (fun x -> int_of_float (ceil (log (float_of_int x))));
  mul = Int.mul;  
  add = ( + );
  of_int = (fun x -> x); 
  of_pqnum = PQNum.to_int;
  to_str = string_of_int;   
}

let float_ops : float math_ops = { 
  safe_div = (/.);  
  log = log; 
  mul = ( *. );
  add = ( +. ); 
  of_int = float_of_int; 
  of_pqnum = PQNum.to_float;
  to_str = string_of_float; 
} 

let int32_safe_div x y = Int32.div (Int32.sub (Int32.add x y) Int32.one) y    

let int32_ops : Int32.t math_ops = { 
  safe_div = int32_safe_div;   
  log = (fun x -> Int32.of_float (ceil (log (Int32.to_float x)))); 
  mul = Int32.mul; 
  add = Int32.add; 
  of_int = Int32.of_int; 
  of_pqnum = PQNum.to_int32; 
  to_str = Int32.to_string;  
}

let eval_pqnum_op op args =
  let types = List.map PQNum.type_of_num args in  
  let commonT = DynType.fold_type_list types in 
  match commonT with 
    | DynType.Float32T -> 
      PQNum.Float32 (eval float_ops op (List.map PQNum.to_float args)) 
    | DynType.Float64T ->
      PQNum.Float64 (eval float_ops op (List.map PQNum.to_float args)) 
    | DynType.Int16T -> 
      PQNum.Int16 (eval int_ops op (List.map PQNum.to_int args))
    | DynType.Int32T -> 
      PQNum.Int32 (eval int32_ops op (List.map PQNum.to_int32 args))
    | _ -> 
      failwith $ Printf.sprintf
        "[MathEval] Evaluation for arguments of type %s not supported"
        (DynType.to_str commonT)  

