(* pp: -parser o pa_macro.cmo *)

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
        if Prim.is_comparison op then 
          fill_elt_type BoolT t3
        else if Prim.is_float_binop op then 
          let resultT = 
            if DynType.sizeof t3 <= DynType.sizeof Float32T then Float32T
            else Float64T 
          in 
          fill_elt_type resultT t3     
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
        
let infer_simple_array_op op argTypes = match op, argTypes with  
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
  | Where, [VecT BoolT] -> VecT Int32T 
  | Where, _ -> failwith "operator 'where' expects a vector of booleans"
  | Index, [VecT a; indexType] ->
    (* can index by any subtype of Int32 or a vector of ints *) 
    (match DynType.common_type indexType Int32T with 
      | Int32T -> a 
      | VecT Int32T -> VecT a 
      | _ -> failwith "wrong index type passed to operator 'index'"
    )  
  | Index, [t; _] when DynType.is_scalar t -> 
    failwith "can't index into a scalar"
  | DimSize, _ -> Int32T
  | Find,  [VecT t1; t2] -> assert (t1 = t2); t1  
  | _ -> 
     failwith $ sprintf 
        "[core_type_infer] Could not infer type for %s\n" 
            (Prim.array_op_to_str op)
            
let infer_adverb op inputTypes = 
  
  match op, inputTypes with
  (* 
     AllPairs operator takes a function 'a, 'b -> 'c^n and two data 
     arguments of type vec 'a, vec 'b, returning n outputs vec (vec 'c)) 
  *) 
  | AllPairs, (fnT::rest) ->
    IFDEF DEBUG THEN 
      assert (DynType.is_function fnT); 
      if List.length rest <> 2 then 
        failwith $ Printf.sprintf 
          "Expected 2 data arguments for AllPairs operator, received: %s\n"
          (DynType.type_list_to_str rest)
      ; 
    ENDIF;  
    let nestedOutTypes = DynType.fn_output_types fnT in
    List.map (fun t -> VecT (VecT t)) nestedOutTypes
    
  (* 
    Map operator takes a function 'a^m -> 'b^m, and input data of type vec 'a
    returning instead vec 'b
  *) 
  | Map, (fnT::rest) ->
      IFDEF DEBUG THEN 
        assert (List.length rest > 0); 
        assert (DynType.is_function fnT);
      ENDIF;  
      let nestedOutTypes = DynType.fn_output_types fnT in 
      List.map (fun nestedOutT -> VecT nestedOutT) nestedOutTypes
           
  | _ -> failwith $ Printf.sprintf 
           "inference not implemented for operator: %s with inputs %s"
           (Prim.array_op_to_str op) 
           (DynType.type_list_to_str inputTypes)

(* given an operator and types of its arguments, return list of types to which *)
(* args must be converted for the operator to work properly *) 
let required_scalar_op_types op argtypes = 
    match (op, argtypes) with
      (* division returns a float *)  
      | op, [t1; t2] when Prim.is_float_binop op -> 
          let t3 = DynType.common_type t1 t2 in
          let resultT = 
            if DynType.sizeof t3 <= DynType.sizeof Float32T then Float32T
            else Float64T 
          in 
          [resultT; resultT]    

      | op, [t1; t2] when Prim.is_binop op ->
          let t3 = DynType.common_type t1 t2 in 
          [t3; t3] 
      | Select, [predT; t1; t2] -> 
          let t3 = DynType.common_type t1 t2 in
          let predT' = DynType.common_type predT BoolT in  
          [predT'; t3; t3] 
      (* upconvert non-float arguments to appropriate float size *) 
      | op, [t]  
        when Prim.is_float_unop op && 
        DynType.sizeof t <= DynType.sizeof Float32T -> [Float32T]
      (* if type doesn't fit in float32 but is scalar use float64 *)
      | op, [t] 
        when Prim.is_float_unop op && DynType.is_scalar t -> [Float64T]
      (* if not a floating unop, just keep type the same *) 
      | op, [t] when Prim.is_unop op -> [t]  
      | _ -> failwith 
            ("no valid coercions for operator " ^ (Prim.scalar_op_to_str op) ^ 
             " with input types " ^ (DynType.type_list_to_str argtypes)) 
