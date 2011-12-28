open Base 
open SSA

type tenv = ImpType.t ID.Map.t  



let infer_value (tenv:tenv) {value; value_type} : ImpType.t =
    match value_type with 
    | Type.ScalarT elt_t -> ImpType.ScalarT elt_t
    | _ -> failwith "non-scalar types not yet implemented"
    

let infer_exp (tenv:tenv) {exp} = match exp with 
    | Values vs -> List.map (infer_value tenv) vs  

let infer_stmt (tenv:tenv) {stmt} = match stmt with 
    | SSA.Set(ids, rhs) -> ID.Map.extend tenv ids (infer_exp tenv rhs)
    | _ -> failwith "SSA statement not implemented yet" 

let infer (fn:SSA.fn) (inputTypes:ImpType.t list) : tenv  = 
    let tenv = ID.Map.of_lists fn.input_ids inputTypes in
    Block.fold_forward infer_stmt tenv fn.body  
    
    
          
