open Imp
open ImpType 
open ImpHelpers 
open ImpCodegen 

(* are these necessary? or should we just register each SSA variable with its existing name as an imp variable*)
(* and then implicitly keep this information on the codegen object? *) 

type id_env = Imp.value_node ID.Map.t
type type_env = ImpType.t ID.Map.t 
type shape_env = SymbolicShape.t ID.Map.t 

type env = { 
  id_env : id_env; 
  type_env : type_env; 
  shape_env : shape_env; 
}

type loop_descr = {
  loop_var : value_node; 
  loop_start : value_node; 
  loop_test_val : value_node; 
  loop_test_cmp : Prim.scalar_op; 
  loop_incr : value_node; 
  loop_incr_op : Prim.scalar_op; 
} 

let rec build_loop_nests codegen (descrs : loop_descr list) (body:Imp.block) =
  match descrs with 
    | [] -> body
    | d::ds ->
        let nested = build_loop_nests codegen ds body in  
        let testEltT = ImpType.elt_type d.loop_var.value_type in 
        let test = {
          exp = Imp.Op(testEltT, d.loop_test_cmp, [d.loop_var; d.loop_test_val]); 
          exp_type = ImpType.bool_t
        }
        in
        let next = {
           exp = Imp.Op(testEltT, d.loop_incr_op, [d.loop_var; d.loop_incr]); 
           exp_type = d.loop_var.value_type;
        }
        in  
        let update = codegen#set d.loop_var next in
        [
          codegen#set_val d.loop_var d.loop_start; 
          Imp.While (test , nested @ [update])   
        ]

let translate_value idEnv valNode = match valNode.SSA.value with 
  | SSA.Var id -> ID.Map.find id idEnv 
  | SSA.Num n -> {value = Imp.Const n; value_type = ImpType.ScalarT (ParNum.type_of n)}
  | _ -> assert false 

let translate_exp codegen idEnv expNode = match expNode.SSA.exp with 
  | SSA.Values [v] -> ImpHelpers.exp_of_val (translate_value idEnv v)
  | SSA.Values _ -> failwith "multiple value expressions not supported"
  | _ -> assert false 

let mk_simple_loop_descr codegen ?(down=false) ?(start=ImpHelpers.zero) ~(stop:Imp.value_node) = 
  { loop_var = codegen#var ~shape:[] int32_t; 
    loop_start = start; 
    loop_test_val = stop; 
    loop_test_cmp = (if down then Prim.Lt else Prim.Gt); 
    loop_incr = (if down then ImpHelpers.int (-1) else ImpHelpers.one);
    loop_incr_op = Prim.Add;  
  }


let rec translate_block (codegen : ImpCodegen.codegen) (idEnv:id_env) block : id_env = 
  Block.fold_forward (translate_stmt codegen) idEnv block
and translate_stmt codegen (idEnv : id_env) stmtNode : id_env = 
  match stmtNode.SSA.stmt with
    | SSA.Set([id], rhs) -> 
      let rhs' = translate_exp codegen idEnv rhs in
      let impVar = codegen#var rhs'.exp_type in
      let idEnv' = ID.Map.add id impVar idEnv in 
      codegen#set impVar rhs'; 
      idEnv   
    | SSA.Set _ -> failwith "multiple assignment not supported" 
    | _ -> assert false 
   
let translate  (ssaFn:SSA.fn) (impInputTypes:ImpType.t list) : Imp.fn = 
  let codegen = new ImpCodegen.fn_codegen in
  let shapeEnv : SymbolicShape.env  = 
    ShapeInference.infer_normalized_shape_env (FnManager.get_typed_function_table ()) ssaFn 
  in
  let impInputs = List.map2 codegen#named_input ssaFn.SSA.input_ids impInputTypes in
  let impTyEnv = InferImpTypes.infer ssaFn impInputTypes in 
  let register_output id = 
    let symShape = ID.Map.find id shapeEnv in
    let impType = ID.Map.find id impTyEnv in   
    codegen#named_output id ~shape:symShape impType 
  in 
  let impOutputs = List.map register_output ssaFn.SSA.output_ids in   
  let idEnv = 
    ID.Map.of_lists  (ssaFn.SSA.input_ids @ ssaFn.SSA.output_ids) (impInputs @ impOutputs) 
  in 
  (* do something for outputs? *)      
  translate_block (codegen :> ImpCodegen.codegen) idEnv ssaFn.SSA.body;
  codegen#finalize_fn  
    
            
                
(*
let translate ssaFn impTypes = assert false 
*)


(*
let translate_map ?(axes=[0]) ~(fn:SSA.fn) ~(args:SSA.value list) : stmts_and_info  =
  let fn' = ImpReplace.fresh_fn fn in  
  let allIds = (fn.input_ids@fn.output_ids@fn.local_ids) in 
  let types = List.map (fun id -> ID.Map.find id fn'.types) allIds in
  let shapes = List.map (fun id -> ID.Map.find id fn'.shapes) allIds in
  let keepGoing = codegen#var ImpType.bool_t  
  let loop = while_ 
 
*)  

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
(*
let rec block_folder (tenv:ImpType.t ID.Map.t) (acc : Imp.stmt list) (ssaStmt : SSA.stmt_node) = 
    match ssaStmt.SSA.stmt with 
      | SSA.Set ([id], rhs) -> 
        assert (List.length ids == 1);  
        Imp.Set (id, translate_exp tenv rhs)
      | _ -> failwith "ssa2imp: not implemented"
let rec translate_block (tenv:ImpType.t ID.Map.t) (block:SSA.block) = 
  Block.fold_backward (block_folder tenv) [] block  

*)
(*

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
    
 *) 
