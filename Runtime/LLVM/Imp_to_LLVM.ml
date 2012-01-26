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
       List.map adjust_output_pointer
         (List.map ImpType_to_lltype.to_lltype outputImpTypes);

    named_values = Hashtbl.create 13;
    builder = Llvm.builder context;
    name = FnId.to_str fn.Imp.id;
  }


let compile_const (t:ImpType.t) (n:ParNum.t) =
  let t' : lltype = ImpType_to_lltype.to_lltype t in
  match n with
  | ParNum.Bool b -> const_int t' (if b then 1 else 0)
  | ParNum.Char c -> const_int t' (Char.code c)
  | ParNum.Int16 i -> const_int t' i
  | ParNum.Int32 i32 -> const_int t' (Int32.to_int i32)
  | ParNum.Int64 i64 -> const_of_int64 t' i64 true
  | ParNum.Float32 f -> const_float t' f
  | ParNum.Float64 f -> const_float t' f
  | _ -> assert false


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
  | Imp.Const const -> compile_const impVal.Imp.value_type const
  | _ -> assert false

let compile_cast (fnInfo:fn_info) original (srcT:ImpType.t) (destT:ImpType.t) =
  if srcT = destT then original
  else begin
    let srcEltT : Type.elt_t = ImpType.elt_type srcT in
    let destEltT : Type.elt_t = ImpType.elt_type destT in
    let destLlvmType : lltype = ImpType_to_lltype.scalar_to_lltype destEltT in
    assert (ImpType.is_scalar destT);
    (* if both ints *)
    let castFn : llvalue -> lltype -> string -> llbuilder -> llvalue =
      if ImpType.is_int srcT && ImpType.is_int destT then (
        if Type.sizeof srcEltT < Type.sizeof destEltT
        then Llvm.build_sext else Llvm.build_trunc
      )
      else if ImpType.is_float srcT && ImpType.is_float destT then (
        if Type.sizeof srcEltT < Type.sizeof destEltT
        then Llvm.build_fpext else Llvm.build_fptrunc
      )
      else if ImpType.is_float srcT && ImpType.is_int destT
           then Llvm.build_fptosi
      else if ImpType.is_int srcT && ImpType.is_float destT
           then Llvm.build_sitofp
      else failwith $
        Printf.sprintf "Unsupported cast from %s to %s\n"
          (ImpType.to_str srcT) (ImpType.to_str destT)
    in
    castFn original destLlvmType "cast_tmp" fnInfo.builder
  end

let compile_cmp (t:Type.elt_t) op (vals:llvalue list) builder =
  let cmpBit = match op, vals with
    | Prim.Eq, [x;y] ->
      if Type.elt_is_int t then
        Llvm.build_icmp Llvm.Icmp.Eq x y "ieqtmp" builder
      else
        Llvm.build_fcmp Llvm.Fcmp.Oeq x y "feqtmp" builder

    | _ ->
      failwith $ Printf.sprintf "Unsupported cmp op %s with %d args"
        (Prim.scalar_op_to_str op) (List.length vals)
  in
  let boolRepr = ImpType_to_lltype.scalar_to_lltype Type.BoolT in
  Llvm.build_zext cmpBit boolRepr "cmp" builder

let compile_math_op (t:Type.elt_t) op (vals:llvalue list) builder =
  match op, vals with
	| Prim.Add, [ x; y ] ->
    if Type.elt_is_int t then Llvm.build_add x y "addtmp" builder
	  else Llvm.build_fadd x y "addftmp" builder
	| Prim.Sub, [ x; y ] ->
    if Type.elt_is_int t then Llvm.build_sub x y "subtmp" builder
    else Llvm.build_fsub x y "subftmp" builder
	| Prim.Mult, [ x; y ] ->
	  if Type.elt_is_int t then Llvm.build_mul x y "multmp" builder
	  else Llvm.build_fmul x y "mulftmp" builder
	| Prim.Div, [ x; y ] ->
     if Type.elt_is_int t then Llvm.build_sdiv x y "sdivtmp" builder
    else Llvm.build_fdiv x y "fdivtmp" builder
  | _ ->
    failwith $ Printf.sprintf "Unsupported math op %s with %d args"
      (Prim.scalar_op_to_str op) (List.length vals)



(* Change to function? *)
let compile_expr fnInfo (impExpr:Imp.exp_node) =
  match impExpr.exp with
  | Imp.Val valNode -> compile_val fnInfo valNode
  | Imp.Op (t, op, vals) ->
      let vals' =  List.map (compile_val fnInfo) vals in
      if Prim.is_comparison op then
        compile_cmp t op vals' fnInfo.builder
      else
        compile_math_op t op vals' fnInfo.builder
 | Imp.Cast(t, v) ->
    let original = compile_val fnInfo v in
    compile_cast fnInfo original v.Imp.value_type t
 | other ->
    failwith $ Printf.sprintf "[Imp_to_LLVM] Not implemented %s\n"
      (Imp.exp_to_str other)

let rec compile_stmt_seq fnInfo currBB = function
  | [] -> currBB
  | head :: tail ->
    let newBB = compile_stmt fnInfo currBB head in
    compile_stmt_seq fnInfo newBB tail

and compile_stmt fnInfo currBB stmt = match stmt with
  | Imp.If (cond, then_, else_) ->
    let llCond = compile_val fnInfo cond in
    let zero = Llvm.const_int (Llvm.type_of llCond) 0 in
    let cond_val =
      Llvm.build_icmp Llvm.Icmp.Ne llCond zero "ifcond" fnInfo.builder
    in
    let the_function = Llvm.block_parent currBB in
    (* Return a new basic block that's empty and positioned after the if *)
    let after_bb = Llvm.append_block context "afterif" the_function in
    let then_bb = Llvm.append_block context "then" the_function in
    Llvm.position_at_end then_bb fnInfo.builder;
    let new_then_bb = compile_stmt_seq fnInfo then_bb then_ in
    let _ = Llvm.build_br after_bb fnInfo.builder in
    let else_bb = Llvm.append_block context "else" the_function in
    Llvm.position_at_end else_bb fnInfo.builder;
    let new_else_bb = compile_stmt_seq fnInfo else_bb else_ in
    let _ = Llvm.build_br after_bb fnInfo.builder in
    Llvm.position_at_end currBB fnInfo.builder;
    let _ = Llvm.build_cond_br cond_val then_bb else_bb fnInfo.builder in
    Llvm.position_at_end after_bb fnInfo.builder;
    after_bb
  | Imp.While (exp, bb) ->
    let the_function = Llvm.block_parent currBB in
    let after_bb = Llvm.append_block context "after" the_function in
    let loop_bb = Llvm.append_block context "loop" the_function in
    let cond_bb = Llvm.append_block context "cond" the_function in
    let _ = Llvm.build_br cond_bb fnInfo.builder in
    Llvm.position_at_end cond_bb fnInfo.builder;
    let llCond = compile_expr fnInfo exp in
    let zero = Llvm.const_int (Llvm.type_of llCond) 0 in
    let cond_val =
      Llvm.build_icmp Llvm.Icmp.Ne llCond zero "whilecond" fnInfo.builder
    in
    let _ = Llvm.build_cond_br cond_val loop_bb cond_bb fnInfo.builder in
    Llvm.position_at_end loop_bb fnInfo.builder;
    let new_loop_bb = compile_stmt_seq fnInfo loop_bb bb in
    let _ = Llvm.build_br cond_bb fnInfo.builder in
    Llvm.position_at_end after_bb fnInfo.builder;
    after_bb
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
      (* in pointers as int64s and then have to cast them to their *)
      (* actual pointer types inside the code *)
      let pointer = Llvm.build_inttoptr param t (varName^"_ptr") fnInfo.builder
      in
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
