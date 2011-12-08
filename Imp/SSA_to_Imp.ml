open Imp 
open ImpHelpers 

type stmts_and_info = { 
    stmts : Imp.stmt list; 
    new_ids : ID.t list; 
    new_types : ImpType.t list;
    new_shapes : SymbolicShape.shape list;
}

class codegen = object (self)
  val mutable types : ImpType.t ID.Map.t = ID.Map.empty 
  val mutable shapes : SymbolicShape.t ID.Map.t  = ID.Map.empty
   
  method var ?(shape=SymbolicShape.scalar) ty = 
    assert (ImpType.rank ty = SymbolicShape.rank shape); 
    let id = ID.gen() in
    types <- ID.Map.add id ty types; 
    shapes <- ID.Map.add id shape shapes; 
    ImpHelpers.var_val ~t:ty id
    
  method var_exp ?(shape=SymbolicShape.scalar) ty = 
    ImpHelpers.exp_of_val (self#var ~shape ty)
     
    
end

let translate_map ?(axes=[0]) ~(fn:SSA.fn) ~(args:SSA.value list) : stmts_and_info  =
  let fn' = ImpReplace.fresh_fn fn in  
  let allIds = (fn.input_ids@fn.output_ids@fn.local_ids) in 
  let types = List.map (fun id -> ID.Map.find id fn'.types) allIds in
  let shapes = List.map (fun id -> ID.Map.find id fn'.shapes) allIds in
  let keepGoing = codegen#var ImpType.bool_t  
  let loop = while_ 
 
    

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
      new_shapes : SymbolicShape.shape list;
      rhs : Imp.exp_node; 
    } 
    
    type value_info = Imp.exp_node
    
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
