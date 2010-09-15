open Base
open Prim
open DynType 
open SSA
open Printf

exception WrongArity



let rec fill_elt_type fill = function  
  | VecT t' -> VecT (fill_elt_type fill t' )
  | TableT map -> TableT (String.Map.map (fill_elt_type fill) map) 
  | _ -> fill 

(* given an operator and the type of its arguments, return 
   the type of the result 
*) 

 
let infer_scalar_op op argTypes = match op, argTypes with 
  | op, [t1;t2] when Prim.is_binop op -> 
     begin match DynType.common_type t1 t2 with 
     | AnyT -> 
        failwith $ sprintf 
          "[infer_binop] Cannot find concrete common type for %s and %s"
          (DynType.to_str t1) (DynType.to_str t2)  
     | t3 -> 
        if Prim.is_comparison op then fill_elt_type BoolT t3
        else t3
     end
  | op, [t1] when Prim.is_unop op ->
      let resultScalarT = DynType.common_type (DynType.elt_type t1) Float32T in  
      if Prim.is_float_unop op then fill_elt_type resultScalarT t1 
      else if op = Not then fill_elt_type BoolT t1
      else t1 
  | Select, [predT; t1; t2] -> 
      if common_type predT BoolT = AnyT then 
        failwith "predicate for Select operator must be a subtype of Bool"  
      else 
        begin match common_type t1 t2 with 
        | AnyT -> failwith $ sprintf 
          "[infer_op] Scalar conditional cannot match %s and %s\n%!"
            (DynType.to_str t1) (DynType.to_str t2)
        | t3 -> 
           if DynType.nest_depth predT <> DynType.nest_depth t3 then 
            failwith 
             "nest depth of predicate must match nest depth of value in Select"
           else t3 
         end
  | _ -> failwith "no operators expected except unops, binops and select"     
  
let infer_unop op t = infer_scalar_op op [t] 
let infer_binop op t1 t2 = infer_scalar_op op [t1; t2]
        
let infer_array_op op argTypes = match op, argTypes with  
  | Til, [t] when DynType.is_scalar t  -> 
      if DynType.common_type t Int32T <> AnyT then VecT Int32T 
      else failwith "operator 'til' requires an integer argument"
  | Til, [_] -> failwith "scalar argument expected for operator 'til'"
  | Til, _ -> raise WrongArity    
  | Rand, [countT; valT] 
    when DynType.is_number valT 
         && DynType.common_type countT Int32T <> AnyT -> VecT valT
  | Rand, [_;_] -> 
      failwith 
      "Can't assign static type to rand[t1;t2] where t1 isn't <: Int32"
  | Rand, _ -> raise WrongArity
  | _ -> 
     failwith $ sprintf 
        "[core_type_infer] Could not infer type for %s\n" 
            (Prim.array_op_to_str op)
    
let infer_op op argTypes = match op with 
  | ScalarOp op -> infer_scalar_op op argTypes 
  | ArrayOp op -> infer_array_op op argTypes 
  | _ -> failwith "type inference not yet implemented for this kind of operator"
  
(* given an operator and types of its arguments, return list of types to which *)
(* args must be converted for the operator to work properly *) 
let required_op_arg_types op argtypes = 
    match (op, argtypes) with 
      | ScalarOp op, [t1; t2] when Prim.is_binop op ->
            let t3 = DynType.common_type t1 t2 in 
            [t3; t3] 
      | ScalarOp Select, [predT; t1; t2] -> 
            let t3 = DynType.common_type t1 t2 in
            let predT' = DynType.common_type predT BoolT in  
            [predT'; t3; t3] 
      (* upconvert non-float arguments to appropriate float size *) 
      | ScalarOp op, [t]  
        when Prim.is_float_unop op && 
        DynType.sizeof t <= DynType.sizeof Float32T -> [Float32T]
      (* if type doesn't fit in float32 but is scalar use float64 *)
      | ScalarOp op, [t] 
        when Prim.is_float_unop op && DynType.is_scalar t -> [Float64T]
      (* if not a floating unop, just keep type the same *) 
      | ScalarOp op, [t] when Prim.is_unop op -> [t]  
      | _ -> failwith 
            ("no valid coercions for operator " ^ (Prim.prim_to_str op) ^ 
             " with input types " ^ (DynType.type_list_to_str argtypes)) 

let rec infer_value (tLookup : ID.t -> DynType.t) = function 
  | Var id -> tLookup id  
  | Num n -> PQNum.type_of_num n
  | Str _ -> StrT
  | Sym _ -> SymT 
  | Unit -> UnitT
  | Lam fundef -> 
      (* since we specialize lambdas at call sites, 
         just use AnyT as a polymorphic wildcard 
      *)
      let inTypes = 
        List.map (fun _ -> AnyT) (List.til (List.length fundef.input_ids)) in
      let outTypes =  
        List.map (fun _ -> AnyT) (List.til (List.length fundef.output_ids)) in
      FnT(inTypes, outTypes)
  | Prim _ -> 
    failwith "Primitives are both polymorphic and variadic -- not implemented "
