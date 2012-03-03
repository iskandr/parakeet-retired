(* pp: -parser o pa_macro.cmo *)
open Base
open Imp
open Llvm
open LlvmType


let mk_int32 i = Llvm.const_int LlvmType.int32_t i
let mk_int64 i = Llvm.const_int LlvmType.int64_t i

let zero_i32 = mk_int32 0
let zero_i64 = mk_int64 0

let context = Llvm.global_context ()
let global_module = Llvm.create_module context "parakeet_module"


type fn_info = {
  input_ids : ID.t list;
  local_ids : ID.t list;
  output_ids : ID.t list;

  imp_types : ImpType.t ID.Map.t;
  named_values : (string, Llvm.llvalue) Hashtbl.t;
  builder : Llvm.llbuilder;
  name : string;

}

let create_fn_info (fn : Imp.fn) =
  {
    input_ids = fn.Imp.input_ids;
    local_ids = fn.Imp.local_ids;
    output_ids = fn.Imp.output_ids;
    imp_types = fn.Imp.types;

    named_values = Hashtbl.create 13;
    builder = Llvm.builder context;
    name = FnId.to_str fn.Imp.id;
  }


module LLVM_Intrinsics =
  LLVM_Intrinsics.MkIntrinsics(struct let m = global_module end)

module Indexing = struct
  let array_field_to_idx = function
    | ArrayData -> 0
    | ArrayShape -> 1
    | ArrayStrides -> 2
    | RangeStart -> 0
    | RangeStop -> 1
    | ShiftData -> 0
    | ShiftAmt -> 1
    | ShiftDim -> 2
    | ShiftDefault -> 3
    | RotData -> 0
    | RotDim -> 1
    | RotAmt -> 2

  (*
  let data_field : llvalue = mk_int32 (array_field_to_idx Imp.ArrayData)
  let strides_field : llvalue = mk_int32 (array_field_to_idx Imp.ArrayStrides)
  let shape_field : llvalue = mk_int32 (array_field_to_idx Imp.ArrayShape)
  *)
  let get_array_field_addr (fnInfo:fn_info) (array:llvalue) field : llvalue =
    IFDEF DEBUG THEN
      Printf.printf "get_array_field_addr %s\n%!"
        (Imp.array_field_to_str field);
      Llvm.dump_value array;
    ENDIF;
    let name = Imp.array_field_to_str field ^ "_field_addr" in
    let indices = [|zero_i32; mk_int32 (array_field_to_idx field) |] in
    Llvm.build_gep array indices name fnInfo.builder

  let get_array_field (fnInfo:fn_info) (array:llvalue) field : llvalue =
    IFDEF DEBUG THEN
      Printf.printf "get_array_field %s\n%!"
        (Imp.array_field_to_str field);
      Llvm.dump_value array;
    ENDIF;
    let addr = get_array_field_addr fnInfo array field in
    Llvm.dump_value addr;
    let name = Imp.array_field_to_str field in
    Llvm.build_load addr name fnInfo.builder

  let get_array_strides_elt (fnInfo:fn_info) (array:llvalue) (idx:int) =
    IFDEF DEBUG THEN
      Printf.printf "get_array_strides_elt: %d\n%!" idx;
      Llvm.dump_value array;
    ENDIF;
	  let strides : llvalue  = get_array_field fnInfo array ArrayStrides in
	  let stridePtr =
	    if idx <> 0 then
	      Llvm.build_gep strides [|mk_int32 idx|] "stride_ptr" fnInfo.builder
	    else
        strides
	  in
	  let name = "stride_" ^ (string_of_int idx) in
	  Llvm.build_load stridePtr name fnInfo.builder

	let get_array_shape_elt (fnInfo:fn_info) (array:llvalue) (idx:int) =
    IFDEF DEBUG THEN
      Printf.printf "get_array_shape_elt: %d\n%!" idx;
      Llvm.dump_value array;
    ENDIF;
   let shape : llvalue  = get_array_field fnInfo array ArrayShape in
   let eltPtr : llvalue =
     if idx <> 0 then
       Llvm.build_gep shape [|mk_int32 idx|] "shape_ptr" fnInfo.builder
     else
       shape
   in
   Llvm.build_load eltPtr ("dim" ^ (string_of_int idx) ^ "_") fnInfo.builder

	let get_array_data (fnInfo:fn_info) (array:llvalue) : llvalue =
    IFDEF DEBUG THEN
      Printf.printf "get_array_data\n%!";
      Llvm.dump_value array;
    ENDIF;
    let dataFieldPtr = get_array_field fnInfo array ArrayData in
	  Llvm.build_load dataFieldPtr "data_addr" fnInfo.builder

	(* convert a list of indices into an address offset *)
	let rec compute_addr_helper
	    (fnInfo:fn_info)
	    (array:Llvm.llvalue)
	    (i:int)
	    (offset:Llvm.llvalue) = function
	  | currIdx :: otherIndices ->
	    if Llvm.is_null currIdx then
	      compute_addr_helper fnInfo array (i+1) offset otherIndices
	    else (
	      let strideVal : llvalue = get_array_strides_elt fnInfo array i in
        Llvm.dump_value strideVal;
	      let currOffset =
	        Llvm.build_mul strideVal currIdx "offset_term" fnInfo.builder
	      in
        Llvm.dump_value currOffset;
	      let newOffset =
	        if Llvm.is_null offset then currOffset
	        else Llvm.build_add offset currOffset "offset" fnInfo.builder
	      in
	      compute_addr_helper fnInfo array (i+1) newOffset otherIndices
      )
	  | [] ->
      Printf.printf "Done with compute_addr_helper\n%!";
      offset

	let compile_arr_idx
	    (array:Llvm.llvalue)
	    (indices:Llvm.llvalue list)
	    (imp_elt_t:Type.elt_t)
	    (fnInfo:fn_info) =
	  let builder = fnInfo.builder in
	  let dataPtr = get_array_data fnInfo array in
	  let offset = compute_addr_helper fnInfo array 0 zero_i32 indices in
	  let offset64 =
	    Llvm.build_zext_or_bitcast offset LlvmType.int64_t "offsetCast" builder
	  in
    Llvm.dump_value offset64;
	  let dataPtr64 =
	    Llvm.build_ptrtoint dataPtr LlvmType.int64_t "basePtrInt" builder
	  in
    Llvm.dump_value dataPtr64;
	  let newAddr64 = Llvm.build_add dataPtr64 offset64 "idxAddrInt" builder in
    Llvm.dump_value newAddr64;
	  Llvm.build_inttoptr newAddr64 (Llvm.type_of dataPtr) "idxAddr" builder

	let compile_range_load
	    (array:Llvm.llvalue)
	    (indices:Llvm.llvalue list)
	    (imp_elt_t:Type.elt_t)
	    (fnInfo:fn_info) =
	    let startPtr =
	      Llvm.build_gep array [|zero_i32;zero_i32|] "gep_start" fnInfo.builder
	    in
	    let start = Llvm.build_load startPtr "start" fnInfo.builder in
	    let idxInt = match indices with
	      | [arg] ->
	        Llvm.build_add start arg "rangeAdd" fnInfo.builder
	      | _ -> failwith $ Printf.sprintf
	        "[Imp_to_LLVM] Calling range with multiple arguments"
	    in
	    let eltType = LlvmType.of_elt_type imp_elt_t in
	    let eltPtrType = Llvm.pointer_type eltType in
	    let idxAddr =
	      Llvm.build_inttoptr idxInt eltPtrType "idxAddr" fnInfo.builder
	    in
	    Llvm.build_load idxAddr "ret" fnInfo.builder

end
open Indexing


let compile_const (t:ImpType.t) (n:ParNum.t) =
  let t' : lltype = LlvmType.of_imp_type t in
  match n with
  | ParNum.Bool b -> const_int t' (if b then 1 else 0)
  | ParNum.Char c -> const_int t' (Char.code c)
  | ParNum.Int16 i -> const_int t' i
  | ParNum.Int32 i32 -> const_int t' (Int32.to_int i32)
  | ParNum.Int64 i64 -> const_of_int64 t' i64 true
  | ParNum.Float32 f -> const_float t' f
  | ParNum.Float64 f -> const_float t' f
  | _ -> failwith "Not a constant"

let compile_cast (fnInfo:fn_info) original (srcT:ImpType.t) (destT:ImpType.t) =
  if srcT = destT then original
  else begin
    let srcEltT : Type.elt_t = ImpType.elt_type srcT in
    let destEltT : Type.elt_t = ImpType.elt_type destT in
    let destLlvmType : lltype = LlvmType.of_elt_type destEltT in
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
      else if ImpType.is_float srcT && ImpType.is_int destT then
        Llvm.build_fptosi
      else if ImpType.is_int srcT && ImpType.is_float destT then
        Llvm.build_sitofp
      else failwith $
        Printf.sprintf "Unsupported cast from %s to %s"
          (ImpType.to_str srcT) (ImpType.to_str destT)
    in
    castFn original destLlvmType "cast_tmp" fnInfo.builder
  end

let compile_cmp (t:Type.elt_t) op (vals:llvalue list) builder =
  let x,y = match vals with
    | [x;y] -> x,y
    | _ ->
      failwith $ Printf.sprintf
        "[compile_cmp] Expected 2 arguments, received %d"
        (List.length vals)
  in
  let cmpFn : llvalue -> llvalue -> string -> llbuilder -> llvalue =
	  match op with
		| Prim.Eq ->
		  if Type.elt_is_int t then Llvm.build_icmp Llvm.Icmp.Eq
		  else Llvm.build_fcmp Llvm.Fcmp.Oeq
		| Prim.Neq ->
		  if Type.elt_is_int t then Llvm.build_icmp Llvm.Icmp.Ne
		  else Llvm.build_fcmp Llvm.Fcmp.One
		| Prim.Lt ->
		  if Type.elt_is_int t then Llvm.build_icmp Llvm.Icmp.Slt
		  else Llvm.build_fcmp Llvm.Fcmp.Olt
		| Prim.Lte ->
		  if Type.elt_is_int t then Llvm.build_icmp Llvm.Icmp.Sle
		  else Llvm.build_fcmp Llvm.Fcmp.Ole
		| Prim.Gt ->
		  if Type.elt_is_int t then Llvm.build_icmp Llvm.Icmp.Sgt
		  else Llvm.build_fcmp Llvm.Fcmp.Ogt
		| Prim.Gte  ->
		  if Type.elt_is_int t then Llvm.build_icmp Llvm.Icmp.Sge
		  else Llvm.build_fcmp Llvm.Fcmp.Oge
		| _ ->
		  failwith (Printf.sprintf "Unsupported cmp %s" (Prim.scalar_op_to_str op))
  in
  let cmpBit = cmpFn x y "cmptmp" builder in
  let boolRepr = LlvmType.of_elt_type Type.BoolT in
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
  | Prim.Neg, [x] ->
    let llvm_type = LlvmType.of_elt_type t in
    if Type.elt_is_int t then
      let zero = Llvm.const_int llvm_type 0 in
      Llvm.build_sub zero x "negtmp" builder
    else
      let zero = Llvm.const_float llvm_type (-0.0) in
      Llvm.build_fsub zero x "negtmp" builder
  | Prim.Sqrt, [x] ->
    let f =
      match t with
      | Type.Float32T -> LLVM_Intrinsics.sqrt32
      | Type.Float64T -> LLVM_Intrinsics.sqrt64
      | _ -> failwith "Unexpected non-float argument to sqrt"
    in
    Llvm.build_call f [|x|] "sqrt" builder
  | Prim.Exp, [x] ->
    let f =
      match t with
      | Type.Float32T -> LLVM_Intrinsics.exp32
      | Type.Float64T -> LLVM_Intrinsics.exp64
      | _ -> failwith "Unexpected non-float argument to exp"
    in
    Llvm.build_call f [|x|] "exp" builder
  | Prim.Ln, [x] ->
    let f = match t with
      | Type.Float32T -> LLVM_Intrinsics.log32
      | Type.Float64T -> LLVM_Intrinsics.log64
      | _ -> failwith "Unexpected non-float argument to log"
    in
    Llvm.build_call f [|x|] "log" builder
  | Prim.Pow, [ x;y ] ->
    let f =
      match t with
      | Type.Float32T -> LLVM_Intrinsics.pow32
      | Type.Float64T -> LLVM_Intrinsics.pow64
      | _ -> failwith "Unexpected non-float arguments to pow"
    in
    Llvm.build_call f [|x;y|] "pow" builder
  | _ ->
    failwith $ Printf.sprintf "Unsupported math op %s with %d args"
      (Prim.scalar_op_to_str op) (List.length vals)


(* Change to function? *)
let rec compile_value ?(do_load=true) fnInfo (impVal:Imp.value_node) =
  IFDEF DEBUG THEN
    Printf.printf "[Imp_to_LLVM.compile_value] %s\n%!"
      (Imp.value_node_to_str impVal)
  ENDIF;
  match impVal.value with
  | Imp.Var id ->
      let name = ID.to_str id in
      begin match Hashtbl.find_option fnInfo.named_values name with
        | None ->
          failwith $"[Imp_to_LLVM] unknown variable " ^ name
        | Some ptr ->
          if do_load then
            let tempName = name ^ "_value" in
            build_load ptr tempName fnInfo.builder
          else ptr
      end

  | Imp.DimSize (arr, idx) ->
    let llvmArr = compile_value ~do_load:false fnInfo arr in
    let llvmIdx = compile_value fnInfo idx in
    let shape = get_array_field fnInfo llvmArr ArrayShape in
    let dimPtr = Llvm.build_gep shape [|llvmIdx|] "dim_ptr" fnInfo.builder in
    Llvm.build_load dimPtr "dim" fnInfo.builder

  | Imp.Const const -> compile_const impVal.Imp.value_type const
  | Imp.Op (t, op, vals) ->
    let vals' =  List.map (compile_value fnInfo) vals in
    if Prim.is_comparison op then
      compile_cmp t op vals' fnInfo.builder
    else
      compile_math_op t op vals' fnInfo.builder
  | Imp.Cast (t, v) ->
    let original = compile_value fnInfo v in
    compile_cast fnInfo original v.Imp.value_type t
  | Imp.Idx (arr, indices) ->
    let llvmArray = compile_value ~do_load:false fnInfo arr in
    let llvmIndices = List.map (compile_value fnInfo) indices in
    begin match arr.value_type with
      | ImpType.RangeT imp_elt_t ->
        compile_range_load llvmArray llvmIndices imp_elt_t fnInfo
      | ImpType.ArrayT (imp_elt_t, imp_int) ->
        let idxAddr = compile_arr_idx llvmArray llvmIndices imp_elt_t fnInfo in
        Llvm.build_load idxAddr "ret" fnInfo.builder
      end
	| _ ->
	  failwith $ Printf.sprintf
      "[Imp_to_LLVM] Not implemented %s"
	    (Imp.value_node_to_str impVal)

and compile_values fnInfo = function
  | [] -> []
  | vNode :: vNodes ->
    let llvmVal = compile_value fnInfo vNode in
    llvmVal :: (compile_values fnInfo vNodes)

let rec compile_stmt_seq fnInfo currBB = function
  | [] -> currBB
  | head :: tail ->
    let newBB = compile_stmt fnInfo currBB head in
    compile_stmt_seq fnInfo newBB tail

and compile_stmt fnInfo currBB stmt =
  IFDEF DEBUG THEN
    Printf.printf "[Imp_to_LLVM.compile_stmt] %s\n%!"
      (Imp.stmt_to_str stmt)
  ENDIF;
  match stmt with
  | Imp.If (cond, then_, else_) ->
    let llCond = compile_value fnInfo cond in
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

  | Imp.While (cond, bb) ->
    let the_function = Llvm.block_parent currBB in
    let after_bb = Llvm.append_block context "after" the_function in
    let loop_bb = Llvm.append_block context "loop" the_function in
    let cond_bb = Llvm.append_block context "cond" the_function in
    let _ = Llvm.build_br cond_bb fnInfo.builder in
    Llvm.position_at_end cond_bb fnInfo.builder;
    let llCond = compile_value fnInfo cond in
    let zero = Llvm.const_int (Llvm.type_of llCond) 0 in
    let cond_val =
      Llvm.build_icmp Llvm.Icmp.Ne llCond zero "whilecond" fnInfo.builder
    in
    let _ = Llvm.build_cond_br cond_val loop_bb after_bb fnInfo.builder in
    Llvm.position_at_end loop_bb fnInfo.builder;
    let new_loop_bb = compile_stmt_seq fnInfo loop_bb bb in
    let _ = Llvm.build_br cond_bb fnInfo.builder in
    Llvm.position_at_end after_bb fnInfo.builder;
    after_bb

  | Imp.Set (id, rhs) ->
    let rhs : Llvm.llvalue = compile_value fnInfo rhs in
    begin match Hashtbl.find_option fnInfo.named_values (ID.to_str id) with
      | None -> failwith  ("unknown variable name " ^ (ID.to_str id))
      | Some register ->
        let instr = Llvm.build_store rhs register fnInfo.builder in
        currBB
    end

  | Imp.SetIdx (arr, indices, rhs) ->
    let arrayPtr : Llvm.llvalue = compile_value ~do_load:false fnInfo arr in
    let indexRegisters : Llvm.llvalue list = compile_values fnInfo indices in
    let imp_elt_t = match arr.value_type with
      | ImpType.ArrayT (imp_elt_t, _) -> imp_elt_t
      | other -> failwith $ Printf.sprintf
        "[Imp_to_LLVM] Unsuported set index for type %s" (ImpType.to_str other)
    in
    let idxAddr = compile_arr_idx arrayPtr indexRegisters imp_elt_t fnInfo in
    let rhsVal = compile_value fnInfo rhs in
    let _ = Llvm.build_store rhsVal idxAddr fnInfo.builder in
    currBB

  | other ->
    failwith $ Printf.sprintf "[Imp_to_LLVM] Unsupported statement %s"
    (Imp.stmt_to_str other)

let allocate_local_array_info
      (fnInfo:fn_info)
      (name:string)
      (impT:ImpType.t)
      (llvmT:lltype) =
  let local = Llvm.build_alloca llvmT (name^"_local") fnInfo.builder in
  let rank = mk_int32 (ImpType.rank impT) in
  Llvm.build_store
    (Llvm.build_array_alloca int32_t rank (name^"_shape") fnInfo.builder)
    (Indexing.get_array_field fnInfo local ArrayShape)
  ;
  Llvm.build_store
    (Llvm.build_array_alloca int32_t rank (name^"_strides") fnInfo.builder)
    (Indexing.get_array_field fnInfo local ArrayStrides)
  ;
  local



let copy_array (fnInfo:fn_info) srcAddr destAddr nelts =
  for i = 0 to nelts - 1 do
    let src =
      Llvm.build_gep srcAddr [|mk_int32 i|] "src" fnInfo.builder
    in
    Llvm.dump_value src;
    let elt = Llvm.build_load src ("elt_" ^ string_of_int i) fnInfo.builder in
    let dest =
      Llvm.build_gep destAddr [|mk_int32 i|] "dest" fnInfo.builder
    in
    Llvm.dump_value dest;
    let storeInstr = Llvm.build_store elt dest fnInfo.builder in
    Llvm.dump_value storeInstr

  done


(* to avoid loads from shape/stride arrays in the heart of tight loops, *)
(* we instead load all of the metadata into stack allocated arrays and*)
(* hope the fields get turned into SSA variables (and thus optimized) *)
let copy_array_metadata (fnInfo:fn_info)  (src:llvalue) (dest:llvalue) rank  =
  let srcData = get_array_data fnInfo src in
  let destDataField = get_array_field_addr fnInfo dest ArrayData in
  let _ = Llvm.build_store srcData destDataField fnInfo.builder in

  let srcShapeField = get_array_field fnInfo src ArrayShape in
  let destShapeField = get_array_field fnInfo dest ArrayShape in
  copy_array fnInfo srcShapeField destShapeField rank;
  let srcStridesField = get_array_field fnInfo src ArrayStrides in
  let destStridesField = get_array_field fnInfo dest ArrayStrides in
  copy_array fnInfo srcStridesField destStridesField rank

let init_local_var (fnInfo:fn_info) (id:ID.t) =
  let impT = ID.Map.find id fnInfo.imp_types in
  let llvmT = LlvmType.of_imp_type impT in
  IFDEF DEBUG THEN
    Printf.printf "Initializing local %s : %s to have lltype %s\n%!"
      (ID.to_str id)
      (ImpType.to_str impT)
      (Llvm.string_of_lltype llvmT)
    ;
  ENDIF;
  let varName = ID.to_str id in
  let stackVal = Llvm.build_alloca llvmT varName fnInfo.builder in
  Hashtbl.add fnInfo.named_values varName stackVal


let init_nonlocal_var (fnInfo:fn_info) (id:ID.t) (param:Llvm.llvalue) =
  let impT = ID.Map.find id fnInfo.imp_types in
  let llvmT = LlvmType.of_imp_type impT in
  IFDEF DEBUG THEN
    Printf.printf "Initializing nonlocal %s : %s to have lltype %s\n%!"
      (ID.to_str id)
      (ImpType.to_str impT)
      (Llvm.string_of_lltype llvmT)
    ;
  ENDIF;
  let varName = ID.to_str id in
  Llvm.set_value_name varName param;
  let isInput = List.mem id fnInfo.input_ids in
  let isScalar = ImpType.is_scalar impT in
  if isInput && isScalar then (
    let stackVal =
      Llvm.build_alloca llvmT varName fnInfo.builder
    in
    let _ = Llvm.build_store param stackVal fnInfo.builder in
    Hashtbl.add fnInfo.named_values varName stackVal
  )
  else
    (* Due to the bizarre layout of GenericValues, we pass *)
    (* in pointers as int64s and then have to cast them to their *)
    (* actual pointer types inside the code *)
    let ptrT = Llvm.pointer_type llvmT in
    let pointer =
      Llvm.build_inttoptr param ptrT (varName^"_ptr") fnInfo.builder
    in
    if isScalar then Hashtbl.add fnInfo.named_values varName pointer
    else (
      let local = allocate_local_array_info fnInfo varName impT llvmT in
      copy_array_metadata fnInfo pointer local (ImpType.rank impT);
      Hashtbl.add fnInfo.named_values varName local
    )



let init_compiled_fn (fnInfo:fn_info) =
  let get_imp_type id =  ID.Map.find id fnInfo.imp_types in
  let get_imp_types ids = List.map get_imp_type ids in

  let impInputTypes = get_imp_types fnInfo.input_ids in
  let impOutputTypes = get_imp_types fnInfo.output_ids in
  let impLocalTypes = get_imp_types fnInfo.local_ids in

  let replace_array_with_int64 t =
    if ImpType.is_scalar t then LlvmType.of_imp_type t
    else LlvmType.int64_t
  in
  let llvmInputTypes = List.map replace_array_with_int64 impInputTypes in
  (* IMPORTANT: outputs are allocated outside the function and the *)
  (* addresses of their locations are passed in *)
  let llvmOutputTypes =
    List.map (fun _ -> LlvmType.int64_t) impOutputTypes
  in
  let paramTypes = llvmInputTypes @ llvmOutputTypes in
  (* since we have to pass output address as int64s, convert them all *)
  (* in the signature *)
  let fnT = Llvm.function_type void_t (Array.of_list paramTypes) in
  let llvmFn = Llvm.declare_function fnInfo.name fnT global_module in
  let bb = Llvm.append_block context "entry" llvmFn in
  Llvm.position_at_end bb fnInfo.builder;
  (* To avoid having to manually encode phi-nodes around the *)
  (* use of mutable variables, we instead allocate stack space *)
  (* for every input and local variable at the beginning of the *)
  (* function. We don't need to allocate space for inputs since *)
  (* they are already given to us as pointers. *)
  List.iter2
    (init_nonlocal_var fnInfo)
    (fnInfo.input_ids @ fnInfo.output_ids)
    (Array.to_list (Llvm.params llvmFn))
  ;
  List.iter (init_local_var fnInfo) fnInfo.local_ids;
  llvmFn

let compile_fn (fn : Imp.fn) : Llvm.llvalue =
  let fnInfo = create_fn_info fn in
  let llvmFn : Llvm.llvalue = init_compiled_fn fnInfo in
  let initBasicBlock : Llvm.llbasicblock = Llvm.entry_block llvmFn in
  let _ = compile_stmt_seq fnInfo initBasicBlock fn.body in
  (* we implement multiple return values by passing the output addresses as *)
  (* parameters so there's nothing left to return *)
  Llvm.build_ret_void fnInfo.builder;
  llvmFn
