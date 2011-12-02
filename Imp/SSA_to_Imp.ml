(*type fn = {
  body: block;
  tenv : tenv;
  input_ids:ID.t list;
  output_ids: ID.t list; 
  fn_input_types : Type.t list;
  fn_output_types : Type.t list;  
  fn_id : FnId.t; 
}

*)


(*
type fn = {
  input_ids : ID.t list;
  output_ids : ID.t list; 
  local_ids : ID.t list;  
  var_info : var_info ID.Map.t; 
  body : block;
}
*)

let rec block_folder (tenv:ImpType.t ID.Map.t) (acc : Imp.stmt list) (ssaStmt : SSA.stmt_node) = 
    match ssaStmt.SSA.stmt with 
      | SSA.Set ([id], rhs) -> 
        assert (List.length ids == 1);  
        Imp.Set (id, translate_exp tenv rhs)
      | _ -> failwith "ssa2imp: not implemented"
let rec translate_block (tenv:ImpType.t ID.Map.t) (block:SSA.block) = 
  Block.fold_backward (block_folder tenv) [] block  



module Analysis = struct 
    type env = Imp.fn
    type exp_info = { 
        stmts : Imp.stmt list; 
        new_ids : ID.t list; 
        new_types : ImpType.t list;
        new_shapes : SymbolicShape.shape list
        result : 
    type value_info
    
    val dir : direction
  
    (* should analysis be repeated until environment stops changing? *) 
    val iterative : bool
  
    val init : fn -> env 
    val value : env -> value_node -> value_info
    
    val exp : env -> exp_node -> (env, value_info) helpers -> exp_info 
    val stmt : env -> stmt_node -> (env, value_info) helpers -> env option
    
    val phi_set : env -> ID.t -> value_info -> env option 
    val phi_merge : env -> ID.t -> value_info -> value_info -> env option 
end

let translate (fnTable:FnTable.t) (fn:SSA.fn) (inputTypes:ImpType.t list) : Imp.fn = 
    let tenv = InferImpTypes.infer fn inputTypes in
    let senv : SymbolicShape.shape ID.Map.t = ShapeInference.infer_shape_env fnTable fn in    
    let body = translate_body tenv fn.SSA.body in  
    { 
        input_ids = fn.SSA.input_ids; 
        output_ids = fn.SSA.output_ids; 
        local_ids = localIds;
        types = tenv; 
        shapes = senv;  
        body = body;       
    }
