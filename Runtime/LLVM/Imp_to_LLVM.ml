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
let global_module = Llvm.create_module context "parakeet_module"

type fn_info = {
  input_ids : ID.t list;
  local_ids : ID.t list;
  output_ids : ID.t list;

  input_imp_types : ImpType.t list;
  local_imp_types : ImpType.t list;
  output_imp_types : ImpType.t list;

  input_llvm_types : Llvm.lltype list;
  local_llvm_types : Llvm.lltype list;
  output_llvm_types : Llvm.lltype list;

  named_values : (string, Llvm.llvalue) Hashtbl.t;
  builder: Llvm.llbuilder;
  name : string;
}

let create_fn_info (fn : Imp.fn) = 
  let inputImpTypes = Imp.input_types fn in
  let localImpTypes = Imp.local_types fn in  
  let outputImpTypes = Imp.output_types fn in 
  {
    input_ids = fn.Imp.input_ids;
    local_ids = fn.Imp.local_ids;
    output_ids = fn.Imp.output_ids;

    input_imp_types = inputImpTypes;
    local_imp_types = localImpTypes;
    output_imp_types = outputImpTypes;

    input_llvm_types = List.map ImpType_to_lltype.to_lltype inputImpTypes;
    local_llvm_types = List.map ImpType_to_lltype.to_lltype localImpTypes;
    (* IMPORTANT: outputs are allocated outside the functiona and the *)
    (* addresses of their locations are passed in *)
    output_llvm_types =
       List.map Llvm.pointer_type
         (List.map ImpType_to_lltype.to_lltype outputImpTypes);

    named_values = Hashtbl.create 13;
    builder = Llvm.builder context;
    name = FnId.to_str fn.Imp.id;
  }

let compile_val (fnInfo:fn_info) (impVal:Imp.value_node) : Llvm.llvalue = 
  match impVal.value with
  | Imp.Var id ->
      let ptr = try Hashtbl.find fnInfo.named_values (ID.to_str id) with
      | Not_found -> failwith "unknown variable name"
      in
      if ImpType.is_scalar impVal.value_type then
        let tempName = (ID.to_str id) ^ "_value" in 
        build_load ptr tempName fnInfo.builder
      else
        ptr
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
    print_endline $ "generating store for " ^ (Imp.stmt_to_str stmt);
    dump_value rhs;
    dump_value instr;
    currBB
  | _ -> assert false

let init_compiled_fn (fnInfo:fn_info) =
  (* since we have to pass output address as int64s, convert them all*)
  (* in the signature *) 
  let paramTypes = replace_pointers
    (fnInfo.input_llvm_types @ fnInfo.output_llvm_types) in
  let fnT = Llvm.function_type void_t (Array.of_list paramTypes) in
  let llvmFn = Llvm.declare_function fnInfo.name fnT global_module in
  let bb = Llvm.append_block context "entry" llvmFn in
  Llvm.position_at_end bb fnInfo.builder;
  (* To avoid having to manually encode phi-nodes around the *)
  (* use of mutable variables, we instead allocate stack space *)
  (* for every input and local variable at the beginning of the *)
  (* function. We don't need to allocate space for inputs since *)
  (* they are already given to us as pointers. *)
  let init_param_var (id:ID.t) (t:Llvm.lltype) (param:Llvm.llvalue) =
    let varName = ID.to_str id in
    Llvm.set_value_name varName param;
    if List.mem id fnInfo.input_ids && not (is_pointer t) then
      let pointer = Llvm.build_alloca t varName fnInfo.builder in
      let _ = Llvm.build_store param pointer fnInfo.builder in
      Hashtbl.add fnInfo.named_values varName pointer
    else
      (* Due to the bizarre layout of GenericValues, we pass *)
      (* in pointers as int64s and then have to cast them to their*)
      (* actual pointer types inside the code *)
      let pointer = Llvm.build_inttoptr param t (varName^"_ptr") fnInfo.builder in  
      Hashtbl.add fnInfo.named_values varName pointer
  in
  List.iter3 init_param_var
    (fnInfo.input_ids @ fnInfo.output_ids)
    (fnInfo.input_llvm_types @ fnInfo.output_llvm_types)
    (Array.to_list (Llvm.params llvmFn))
  ;
  let init_local_var (id:ID.t) (t:Llvm.lltype) =
    let varName = ID.to_str id in
    let pointer = Llvm.build_alloca t varName fnInfo.builder in
    Hashtbl.add fnInfo.named_values varName pointer
  in
  List.iter2 init_local_var fnInfo.local_ids fnInfo.local_llvm_types
  ;
  llvmFn

let compile_fn (fn : Imp.fn) : Llvm.llvalue =
  let fnInfo = create_fn_info fn in
  let llvmFn : Llvm.llvalue =   init_compiled_fn fnInfo in
  let initBasicBlock : Llvm.llbasicblock = Llvm.entry_block llvmFn in
  let _ : Llvm.llbasicblock = compile_stmt_seq fnInfo initBasicBlock fn.body in
  (* we implement multiple return values by passing the output addresses as *)
  (* parameters so there's nothing left to return *)
  Llvm.build_ret_void fnInfo.builder;
  llvmFn
