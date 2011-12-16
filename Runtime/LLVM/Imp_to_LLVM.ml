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
let the_module = Llvm.create_module context "my_module"
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
type function_record = { vars: ID.t list;
                         types: Llvm.lltype list;
                         builder: Llvm.llbuilder}


let create_llvm_vars inputs locals outputs = 
  List.concat [inputs;locals;outputs]

let create_llvm_types inputTypes localTypes outputTypes = 
  let impTypes = List.concat [inputTypes;localTypes;outputTypes] in
  List.map (fun impType -> ImpType_to_lltype.to_lltype impType) impTypes

let codegen_proto llvmVars llvmTypes name =
  let ft = Llvm.function_type Llvm.void llvmTypes in
  let f = Llvm.declare_function name ft the_module in
  let params = Array.to_list Llvm.params f in
  List.iter2 (fun var param ->
    let varName = ID.to_str var in
    Llvm.set_value_name varName param;
    Hashtbl.add named_values varName param
  ) (llvmVars params);
  f

let create_argument_allocas theFunction llvmVars llvmTypes =
  let params = Array.to_list Llvm.params theFunction in
  List.iter2 (fun var param ->
    let varName = ID.to_str var in
    let alloca = Llvm.create_entry_block_alloca theFunction varName in
    ignore(Llvm.build_store param alloca builder);
    Hashtbl.add named_values varName alloca;
  ) (llvmVars params)

let init_compiled_fn llvmVars llvmTypes name builder =
  Hashtbl.clear named_values; 
  let theFunction = codegen_proto llvmVars llvmTypes name in
  let bb = Llvm.append_block context "entry" theFunction in
  Llvm.position_at_end bb builder;
  begin try
    create_argument_allocas theFunction llvmVars llvmTypes
  with e ->
    Llvm.delete_function theFunction;
    raise e
  end; 
  bb

let compile_val (impVal:Imp.value_node) = match impVal.value with
  | Imp.Var id ->
      let v = try Hashtbl.find named_values (ID.to_str id) with
      | Not_found -> failwith "unknown variable name"
      in
      v
  | Imp.Const const -> Value_to_llvalue.to_llvm const
  | _ -> assert false

let compile_expr fnInfo (impExpr:Imp.exp_node) = 
  match impExpr.exp with
  | Imp.Val valNode -> compile_val valNode 
  | Imp.Op (t, op, vals) ->
      let vals' =  List.map compile_val vals in 
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
          | Type.Int64T -> Llvm.build_mul x rhs "multmp" fnInfo.builder
          | Type.Float32T
          | Type.Float64T -> Llvm.build_fmul y rhs "mulftmp" fnInfo.builder
          end
        | Prim.Div, [ x; y ] -> Llvm.build_sdiv x y "sdivtmp" fnInfo.builder
      )
 | _ -> assert false


let rec compile_stmt_seq fnInfo currBB = function 
  | [] -> currBB
  | head :: tail ->
    let newBB = compile_stmt fnInfo currBB head in
    compile_stmt_seq fnInfo newBB tail
    
and compile_stmt fnInfo currBB = function
  | Imp.Set (id, exp) ->
    let rhs = compile_expr fnInfo exp in
    let variable = try Hashtbl.find named_values (ID.to_str id) with
    | Not_found -> raise (Error "unknown variable name " ^ (ID.to_str id))
    in
    ignore(Llvm.build_store rhs variable fnInfo.builder);
    currBB
  | _ -> assert false


let compile_fn fn =
  let builder = Llvm.builder context in
  let inputTypes = List.map (fun id -> ID.Map.find id fn.types) fn.input_ids in
  let localTypes = List.map (fun id -> ID.Map.find id fn.types) fn.local_ids in
  let outputTypes = List.map (fun id -> ID.Map.find id fn.types) fn.output_ids in
  
  let llvmVars = create_llvm_vars fn.input_ids fn.local_ids fn.output_ids in
  let llvmTypes = create_llvm_types inputTypes localTypes outputTypes in
  let fnInfo = { vars = llvmVars; types = llvmTypes; builder = builder } in
  let initBasicBlock = init_compiled_fn llvmVars llvmTypes (FnId.to_str fn.id) builder in
  let firstBasicBlock = compile_stmt_seq fnInfo initBasicBlock in
  firstBasicBlock
