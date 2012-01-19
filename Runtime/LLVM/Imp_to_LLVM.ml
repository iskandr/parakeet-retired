open Base 
open Imp 
open Llvm 
open LLVM_Types 

let array_field_to_idx = function  
  | RangeStart -> 0
  | RangeStop -> 1
  | ShiftData -> 0
  | ShiftAmt -> 1
  | ShiftDim -> 2
  | ShiftDefault -> 3 
  | RotData -> 0
  | RotDim -> 1
  | RotAmt -> 2
  | SliceData -> 0
  | SliceDim -> 1
  | SliceStart -> 2
  | SliceStop -> 3
  | FrozenData -> 0
  | FrozenDim -> 1
  | FrozenIdx -> 2


let context = Llvm.global_context ()
let global_module = Llvm.create_module context "my_module"


type fn_info = { 
  named_values : (string, Llvm.llvalue) Hashtbl.t;  
  builder: Llvm.llbuilder;
  name : string;  
}

let create_fn_info (fnId:FnId.t) = {
  named_values = Hashtbl.create 13;
  builder = Llvm.builder context; 
  name = FnId.to_str fnId;
}
  

let codegen_proto (fnInfo:fn_info) (llvmVars : ID.t list) (llvmTypes : Llvm.lltype array)  : Llvm.llvalue =
  let ft = Llvm.function_type void_t llvmTypes in
  let f = Llvm.declare_function fnInfo.name ft global_module in
  let params = Array.to_list  (Llvm.params f) in
  List.iter2 (fun var param ->
    let varName = ID.to_str var in
    Llvm.set_value_name varName param;
    Hashtbl.add fnInfo.named_values varName param
  ) llvmVars params
  ;
  f
  
let create_argument_allocas theFunction (fnInfo:fn_info) (vars : ID.t list) (llvmTypes:Llvm.lltype list) : unit =
  Llvm.position_builder (Llvm.instr_begin (Llvm.entry_block theFunction)) fnInfo.builder;  
  let params : Llvm.llvalue list = Array.to_list (Llvm.params theFunction) in
  List.iter2 (fun (var, varType) param ->
    let varName = ID.to_str var in  
    let alloca = Llvm.build_alloca varType varName fnInfo.builder in
    let instr = Llvm.build_store param alloca fnInfo.builder in
    print_endline $ "[create_argument_allocas] generating store for " ^ (ID.to_str var);    
    dump_value instr;  

    Hashtbl.add fnInfo.named_values varName alloca;
  ) (List.combine vars llvmTypes)  params

let create_local_allocas theFunction (fnInfo:fn_info) (vars : ID.t list) (llvmTypes : Llvm.lltype list) = 
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block theFunction)) in
  List.iter2 (fun var typ ->
    let varName = ID.to_str var in
    let alloca = Llvm.build_alloca typ varName builder in
    print_endline $ "[create_local_allocas] generating store for " ^ (ID.to_str var);    
    dump_value alloca;  
    
    Hashtbl.add fnInfo.named_values varName alloca;
  ) vars llvmTypes 

let compile_val (fnInfo:fn_info) (impVal:Imp.value_node) : Llvm.llvalue = 
  match impVal.value with
  | Imp.Var id ->
      let ptr = try Hashtbl.find fnInfo.named_values (ID.to_str id) with
      | Not_found -> failwith "unknown variable name"
      in
      let tempName = (ID.to_str id) ^ "_value" in 
      build_load ptr tempName fnInfo.builder  
  | Imp.Const const -> Value_to_llvalue.parnum_to_llvm const
  | _ -> assert false

(* Change to function? *)
let compile_expr fnInfo (impExpr:Imp.exp_node) = 
  match impExpr.exp with
  | Imp.Val valNode -> compile_val fnInfo valNode 
  | Imp.Op (t, op, vals) ->
      let vals' =  List.map (compile_val fnInfo) vals in 
      ( match op, vals' with
        | Prim.Add, [ x; y ] ->
          begin match t with
          | Type.Int16T
          | Type.Int32T
          | Type.Int64T -> Llvm.build_add x y "addtmp" fnInfo.builder
          | Type.Float32T
          | Type.Float64T -> Llvm.build_fadd x y "addftmp" fnInfo.builder
          end
        | Prim.Sub, [ x; y ] ->
          begin match t with
          | Type.Int16T
          | Type.Int32T
          | Type.Int64T -> Llvm.build_sub x y "subtmp" fnInfo.builder
          | Type.Float32T
          | Type.Float64T -> Llvm.build_fsub x y "subftmp" fnInfo.builder
          end
        | Prim.Mult, [ x; y ] ->
          begin match t with
          | Type.Int16T
          | Type.Int32T
          | Type.Int64T -> Llvm.build_mul x y "multmp" fnInfo.builder
          | Type.Float32T
          | Type.Float64T -> Llvm.build_fmul x y "mulftmp" fnInfo.builder
          end
        | Prim.Div, [ x; y ] -> Llvm.build_sdiv x y "sdivtmp" fnInfo.builder
      )
 | _ -> assert false


let rec compile_stmt_seq fnInfo currBB = function 
  | [] -> currBB
  | head :: tail ->
    let newBB = compile_stmt fnInfo currBB head in
    compile_stmt_seq fnInfo newBB tail
    
and compile_stmt fnInfo currBB stmt = match stmt with 
  | Imp.Set (id, exp) ->

    let rhs = compile_expr fnInfo exp in
    let variable = try Hashtbl.find fnInfo.named_values (ID.to_str id) with
    | Not_found -> failwith  ("unknown variable name " ^ (ID.to_str id))
    in
    let instr = Llvm.build_store rhs variable fnInfo.builder in 
    print_endline $ "generating store for " ^  (Imp.stmt_to_str stmt); 
    dump_value rhs; 
    dump_value instr; 
    currBB
  | _ -> assert false


let init_compiled_fn fnInfo ~local_ids ~local_types ~input_ids ~input_types =
  let theFunction = codegen_proto fnInfo input_ids (Array.of_list input_types)  in
  let bb = Llvm.append_block context "entry" theFunction in
  Llvm.position_at_end bb fnInfo.builder;
  begin try
    create_argument_allocas theFunction fnInfo input_ids input_types
  with e ->
    Llvm.delete_function theFunction;
    raise e
  end; 
  create_local_allocas theFunction fnInfo local_ids local_types; 
  theFunction


let add_output_return fnInfo output_ids = 
  if List.length output_ids == 1 then
    let output_id = List.hd output_ids in
    let variable = try Hashtbl.find fnInfo.named_values (ID.to_str output_id) with
    | Not_found -> failwith  ("unknown variable name " ^ (ID.to_str output_id))
    in
    Llvm.build_ret variable fnInfo.builder
  else
    failwith "multiple returns are not supported"

let compile_fn (fn : Imp.fn) : Llvm.llvalue =
  let inputImpTypes = List.map (fun id -> ID.Map.find id fn.types) fn.input_ids in
  let localImpTypes = List.map (fun id -> ID.Map.find id fn.types) fn.local_ids in
  let outputImpTypes = List.map (fun id -> ID.Map.find id fn.types) fn.output_ids in
  (* concat true locals and local version of output vars *) 
  let localVarIds : ID.t list = fn.local_ids @ fn.output_ids in
  let impTypes = localImpTypes @ outputImpTypes in 
  let localTypes : Llvm.lltype list = 
    List.map (fun impType -> ImpType_to_lltype.to_lltype impType) impTypes
  in 
  let inputTypes : Llvm.lltype list = 
    List.map (fun impType -> ImpType_to_lltype.to_lltype impType) inputImpTypes 
  in
  let fnInfo = create_fn_info fn.id in  
  let llvmFn : Llvm.llvalue = 
    init_compiled_fn 
      fnInfo
      ~input_ids:fn.input_ids
      ~input_types:inputTypes  
      ~local_ids:localVarIds
      ~local_types:localTypes
  in 
  let initBasicBlock : Llvm.llbasicblock = Llvm.entry_block llvmFn in 
  let _ : Llvm.llbasicblock = compile_stmt_seq fnInfo initBasicBlock fn.body in
  add_output_return fnInfo fn.output_ids;
  llvmFn 
