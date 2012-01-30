open Base 

(* generic evaluator for scalar operations, used in both Eval and ShapeEval *)

type 'a math_ops = {  
  of_int : int -> 'a; 
  of_pqnum : ParNum.t -> 'a;
  to_str : 'a -> string; 
  
  safe_div : 'a -> 'a -> 'a;  
  log : 'a -> 'a; 

  mul : 'a -> 'a -> 'a; 
  add : 'a -> 'a -> 'a;    
  sub : 'a -> 'a -> 'a; 
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
  | Prim.Sub, [x;y] -> m.sub x y  
  | Prim.SafeDiv, [x;y] ->  m.safe_div x y 
  | Prim.Mult, [x;y] -> m.mul x y  
  | Prim.Ln, [x]  -> m.log x
  | op, args -> failwith $ 
     Printf.sprintf "[MathEval] Operations %s not supported with arguments %s"
       (Prim.scalar_op_to_str op)
       (String.concat ", " (List.map m.to_str args)) 
  


let int_ops : int math_ops = {  
  safe_div = safe_div; 
  log =  (fun x -> int_of_float (ceil (log (float_of_int x))));
  mul = Int.mul;  
  add = ( + );
  sub = ( - ); 
  of_int = (fun x -> x); 
  of_pqnum = ParNum.to_int;
  to_str = string_of_int;   
}

let float_ops : float math_ops = { 
  safe_div = (/.);  
  log = log; 
  mul = ( *. );
  add = ( +. );
  sub = ( -. );  
  of_int = float_of_int; 
  of_pqnum = ParNum.to_float;
  to_str = string_of_float; 
} 

let int32_safe_div x y = Int32.div (Int32.sub (Int32.add x y) Int32.one) y    

let int32_ops : Int32.t math_ops = { 
  safe_div = int32_safe_div;   
  log = (fun x -> Int32.of_float (ceil (log (Int32.to_float x)))); 
  mul = Int32.mul; 
  add = Int32.add;
  sub = Int32.sub;  
  of_int = Int32.of_int; 
  of_pqnum = ParNum.to_int32; 
  to_str = Int32.to_string;  
}

let eval_pqnum_op op args =
  let types = List.map ParNum.type_of args in  
  match Type.common_elt_type_list types with 
    | Type.Float32T -> 
      ParNum.Float32 (eval float_ops op (List.map ParNum.to_float args)) 
    | Type.Float64T ->
      ParNum.Float64 (eval float_ops op (List.map ParNum.to_float args)) 
    | Type.Int16T -> 
      ParNum.Int16 (eval int_ops op (List.map ParNum.to_int args))
    | Type.Int32T -> 
      ParNum.Int32 (eval int32_ops op (List.map ParNum.to_int32 args))
    | commonT -> 
      failwith $ Printf.sprintf
        "[MathEval] Evaluation for arguments of type %s not supported"
        (Type.elt_to_str commonT)  

