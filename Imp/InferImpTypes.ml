(* pp: -parser o pa_macro.cmo *)
open Base 
open SSA

type tenv = ImpType.t ID.Map.t  

let infer_value (tenv:tenv) {value; value_type} : ImpType.t =
  if Type.is_scalar value_type then ImpType.ScalarT (Type.elt_Type value_type)
  else match value with 
    | Imp.Var id -> 
	    assert (ID.Map.mem id tenv); 
	    ID.Map.find id tenv
    | Imp.Const n -> ImpType.ScalarT (ParNum.type_of n)
    | Imp.CudaInfo _ -> ImpType.int32_t 

let infer_exp (tenv:tenv) {exp} : ImpType.t list = 
  match exp with 
  | Values vs -> List.map (infer_value tenv) vs  
  | App _ -> failwith "[InferImpTypes] Unexpected untyped function application" 
  | Arr vs -> failwith "[InferImpTypes] Array literals not yet implemented"
  | Cast (t, v) -> 
    (* assume casts are always between scalar types *) 
	assert (Type.is_scalar t); 
    [ImpType.ScalarT (Type.elt_type t)]
  | Call (fnId, args) -> failwith "[InferImpTypes] Typed function calls not implemented" 
  | PrimApp (prim, args) -> failwith "[InferImpTypes] Primitives not implemented"   
  | Adverb (adverb, closure, adverb_args) ->
   
let rec infer_stmt (tenv:tenv) {stmt} = match stmt with 
  | SSA.Set(ids, rhs) -> ID.Map.extend tenv ids (infer_exp tenv rhs)
  | _ -> failwith "SSA statement not implemented yet" 
and infer_block (tenv:tenv) block = 
  Block.fold_forward infer_stmt tenv block 

let infer (fn:SSA.fn) (inputTypes:ImpType.t list) : tenv  = 
  let tenv = ID.Map.of_lists fn.input_ids inputTypes in
  infer_block tenv fn.body 