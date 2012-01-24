open Base 
open Imp
open ImpType 
open ImpHelpers 
open ImpCodegen 

(* are these necessary? or should we just register each SSA variable with its existing name as an imp variable*)
(* and then implicitly keep this information on the codegen object? *) 


type loop_descr = {
  loop_var : value_node; 
  loop_start : value_node; 
  loop_test_val : value_node; 
  loop_test_cmp : Prim.scalar_op; 
  loop_incr : value_node; 
  loop_incr_op : Prim.scalar_op; 
} 

let rec build_loop_nests (codegen:ImpCodegen.codegen) (descrs : loop_descr list) (body:Imp.block) =
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
        let update = set d.loop_var next in
        [
          set_val d.loop_var d.loop_start; 
          Imp.While (test , nested @ [update])   
        ]

let translate_value (codegen:ImpCodegen.codegen) valNode = match valNode.SSA.value with 
  | SSA.Var id -> codegen#var id 
  | SSA.Num n -> {value = Imp.Const n; value_type = ImpType.ScalarT (ParNum.type_of n)}
  | other -> failwith $ "[ssa->imp] unrecognized value: " ^ (SSA.value_to_str other) 

let translate_exp (codegen:ImpCodegen.codegen) expNode : Imp.exp_node  = 
  match expNode.SSA.exp with 
  | SSA.Values [v] -> ImpHelpers.exp_of_val (translate_value codegen v)
  | SSA.Values _ -> failwith "multiple value expressions not supported"
  | SSA.PrimApp (Prim.ScalarOp op, args) -> 
    let args' = List.map (translate_value codegen) args in 
    let eltT = Type.elt_type (List.hd expNode.SSA.exp_types) in 
    { exp = Op( eltT, op, args'); exp_type = ImpType.ScalarT eltT }
  | SSA.PrimApp _ -> failwith "unsupported primitive" 
    
     
  | _ -> failwith $ "[ssa->imp] unrecognized exp: " ^ (SSA.exp_to_str expNode)  

let mk_simple_loop_descr 
        (codegen:ImpCodegen.codegen) 
        ?(down=false) 
        ?(start=ImpHelpers.zero) 
        ~(stop:Imp.value_node) = 
  { loop_var = codegen#fresh_local int32_t; 
    loop_start = start; 
    loop_test_val = stop; 
    loop_test_cmp = (if down then Prim.Lt else Prim.Gt); 
    loop_incr = (if down then ImpHelpers.int (-1) else ImpHelpers.one);
    loop_incr_op = Prim.Add;  
  }


let rec translate_block (codegen : ImpCodegen.codegen) block : Imp.stmt list = 
  Block.fold_forward (fun acc stmt -> acc @ (translate_stmt codegen stmt)) [] block
and translate_stmt (codegen : ImpCodegen.codegen) stmtNode : Imp.stmt list  = 
  match stmtNode.SSA.stmt with
    | SSA.Set([id], rhs) -> 
      let rhs' = translate_exp codegen rhs in
      let impVar = codegen#var id in
      [set impVar rhs'] 
    | SSA.Set _ -> failwith "multiple assignment not supported" 
    | _ -> assert false 
   
let translate  (ssaFn:SSA.fn) (impInputTypes:ImpType.t list) : Imp.fn = 
  let codegen = new ImpCodegen.fn_codegen in
  let impTyEnv = InferImpTypes.infer ssaFn impInputTypes in 
  let shapeEnv : SymbolicShape.env  = 
    ShapeInference.infer_normalized_shape_env (FnManager.get_typed_function_table ()) ssaFn 
  in
  let declare_var (id, impType) : unit = 
    if List.mem id ssaFn.SSA.input_ids then 
      codegen#declare_input id impType
    else ( 
      (* inputs all have the trivial shape 
          SHAPE(x) = [dim(x,0), dim(x,1), etc...]
         but outputs and locals actually have non-trivial
         shapes which need to be declared
      *) 
      let symShape = ID.Map.find id shapeEnv in
      if List.mem id ssaFn.SSA.output_ids then    
        codegen#declare_output id ~shape:symShape impType 
      else 
        codegen#declare id ~shape:symShape impType
    ) 
  in 
  List.iter declare_var (ID.Map.to_list impTyEnv);  
  let body = translate_block (codegen :> ImpCodegen.codegen) ssaFn.SSA.body in 
  codegen#finalize_fn body
