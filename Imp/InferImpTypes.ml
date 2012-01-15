(* pp: -parser o pa_macro.cmo *)
open Base 
open SSA

type tenv = ImpType.t ID.Map.t  

let infer_value (tenv:tenv) {value; value_type} : ImpType.t =
  if Type.is_scalar value_type then 
    ImpType.ScalarT (Type.elt_type value_type)
  else match value with 
    | SSA.Var id -> 
	    if not $ ID.Map.mem id tenv then 
        failwith $ "ID not found: " ^ ID.to_str id
      else  
	      ID.Map.find id tenv
     
    | Num n ->  ImpType.ScalarT (ParNum.type_of n)
    | other -> failwith $ "[ImpInferTypes] invalid value: " ^ (SSA.value_to_str other)  

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
  | Adverb (adverb, closure, adverb_args) -> failwith "[InferImpTypes] adverbs not implemented"
   
let rec infer_stmt (tenv:tenv) {stmt} = match stmt with 
  | SSA.Set(ids, rhs) -> ID.Map.extend tenv ids (infer_exp tenv rhs)
  | other -> failwith $ "SSA statement not implemented yet"

and infer_block (tenv:tenv) block = 
  Block.fold_forward infer_stmt tenv block 

let infer (fn:SSA.fn) (inputTypes:ImpType.t list) : tenv  = 
  let tenv = ID.Map.of_lists fn.input_ids inputTypes in
  infer_block tenv fn.body 