(* pp: -parser o pa_macro.cmo *)

open Base
open Printf
open SSA
open Type

exception WrongArity

let infer_unop op t =
  let resultScalarT = Type.common_elt_type (Type.elt_type t) Float32T in
  if Prim.is_float_unop op then Type.fill_elt_type t resultScalarT
  else if op = Prim.Not then fill_elt_type t BoolT
  else t

let infer_binop op t1 t2 =
  match Type.common_type t1 t2 with
  | AnyT ->
    failwith $ sprintf
          "[infer_binop] Cannot find concrete common type for %s and %s"
          (Type.to_str t1) (Type.to_str t2)
  | t3 ->
    if Prim.is_comparison op then Type.fill_elt_type t3 BoolT
    else if Prim.is_float_binop op then
        let eltResultT =
            if Type.sizeof (Type.elt_type t3) <= Type.sizeof Float32T
            then Float32T
            else Float64T
        in
        fill_elt_type  t3 eltResultT
    else t3

let infer_select predT t1 t2 =
  if common_type predT Type.bool = AnyT then
    failwith "predicate for Select operator must be a subtype of Bool"
  else match common_type t1 t2 with
    | AnyT ->
        failwith $ sprintf
            "[infer_op] Scalar conditional cannot match %s and %s\n%!"
            (Type.to_str t1)
            (Type.to_str t2)
    | t3 ->
        if Type.rank predT <> Type.rank t3 then
            failwith  "nest depth of predicate must match rank of value"
        else t3


let infer_scalar_op op argTypes = match op, argTypes with
  | op, [t1;t2] when Prim.is_binop op -> infer_binop op t1 t2
  | op, [t] when Prim.is_unop op -> infer_unop op t
  | Prim.Select, [predT; t1; t2] -> infer_select predT t1 t2
  | other, types ->
      failwith
        (Printf.sprintf
          "can't infer type for %s with args %s, not a scalar operation"
          (Prim.scalar_op_to_str other)
          (Type.type_list_to_str types))


let infer_indexing_result eltT rank indexTypes =
  let nIndices = List.length indexTypes in
  let resultRank = rank - nIndices in
  (* this will be the result if we're indexing only scalars *)
  if List.for_all Type.is_scalar indexTypes then
    Type.mk_array_type eltT resultRank
  else match indexTypes with
    | [Type.ArrayT(BoolT, 1)]
    | [Type.ArrayT(Int32T, 1)] -> Type.ArrayT(eltT, rank)
    | _ ->
      failwith $ Printf.sprintf
        "[TypeInfer] unsupported indices: %s"
        (Type.type_list_to_str indexTypes)


let infer_simple_array_op op argTypes = match op, argTypes with

  | Prim.Range, [t] when Type.is_scalar t  ->
      if Type.common_type t Type.int32  <> AnyT then Type.ArrayT(Int32T, 1)
      else failwith "operator 'til' requires an integer argument"
  | Prim.Range, [_] -> failwith "scalar argument expected for operator 'til'"
  | Prim.Range, _ -> raise WrongArity
  | Prim.Where, [Type.ArrayT(BoolT, 1)] -> Type.ArrayT(Int32T, 1)
  | Prim.Where, _ -> failwith "operator 'where' expects a vector of booleans"
  | Prim.Index, Type.ArrayT(eltT, rank)::indexTypes ->
      infer_indexing_result eltT rank indexTypes
  | Prim.Index, [t; _] when Type.is_scalar t ->
    failwith "[TypeInfer] can't index into a scalar"
  | Prim.DimSize, _ -> Type.ScalarT Int32T
  | Prim.Find,  [Type.ArrayT(elt_t1, 1); ScalarT elt_t2] ->
        assert (elt_t1 = elt_t2);
        Type.ScalarT elt_t1
  | _ ->
     failwith $ sprintf
        "[core_type_infer] Could not infer type for %s"
            (Prim.array_op_to_str op)

let infer_adverb op inputTypes =

  match op, inputTypes with

  | _ -> failwith $ Printf.sprintf
           "inference not implemented for operator: %s with inputs %s"
           (Prim.adverb_to_str op)
           (Type.type_list_to_str inputTypes)

(* given an operator and types of its arguments, return list of types to which *)
(* args must be converted for the operator to work properly *)
let required_scalar_op_types op argtypes =
    match (op, argtypes) with
      (* division returns a float *)
      | op, [t1; t2] when Prim.is_float_binop op ->
          let t3 = Type.common_type t1 t2 in
          let resultT =
            if Type.sizeof (Type.elt_type t3) <= Type.sizeof Float32T
            then Type.float32
            else Type.float64
          in

          [resultT; resultT]

      | op, [t1; t2] when Prim.is_binop op ->
          let t3 = Type.common_type t1 t2 in
          [t3; t3]
      | Prim.Select, [predT; t1; t2] ->
          let t3 = Type.common_type t1 t2 in
          let predT' = Type.common_type predT Type.bool in
          [predT'; t3; t3]
      (* upconvert non-float arguments to appropriate float size *)
      | op, [t]  when Prim.is_float_unop op &&
          Type.sizeof (Type.elt_type t) <= Type.sizeof Float32T-> [Type.float32]
      (* if type doesn't fit in float32 but is scalar use float64 *)
      | op, [t] when Prim.is_float_unop op && Type.is_scalar t -> [Type.float64]
      (* if not a floating unop, just keep type the same *)
      | op, [t] when Prim.is_unop op -> [t]
      | _ -> failwith
            ("no valid coercions for operator " ^ (Prim.scalar_op_to_str op) ^
             " with input types " ^ (Type.type_list_to_str argtypes))

(*
let infer_q_op qOp argTypes = match qOp, argTypes with
  (* find index of 2nd argument within the first *)
  | Prim.Q_Question, [Type.ArrayT (elt_t1, r); t2]
    when Type.common_type t1 t2 <> Type.AnyT -> Type.Int32T
  | _ -> assert false

let translate_q_op qOp argTypes = match qOp, argTypes with
  | Prim.Q_Question, [Type.VecT t1; t2]
    when Type.common_type t1 t2 <> Type.AnyT -> Prim.ArrayOp Prim.Find
  | _ -> assert false
*)
