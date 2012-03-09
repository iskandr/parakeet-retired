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
let llvm_module = Llvm.create_module context "parakeet_module"

type fn_info = {
  input_ids : ID.t list;
  local_ids : ID.t list;
  output_ids : ID.t list;

  imp_types : ImpType.t ID.Map.t;
  imp_shapes : SymbolicShape.t ID.Map.t;
  imp_storage : Imp.storage ID.Map.t;

  named_values : (string, Llvm.llvalue) Hashtbl.t;
  builder : Llvm.llbuilder;
  name : string;
}

let create_fn_info (fn : Imp.fn) = {
  input_ids = fn.Imp.input_ids;
  local_ids = fn.Imp.local_ids;
  output_ids = fn.Imp.output_ids;
  imp_types = fn.Imp.types;
  imp_shapes = fn.Imp.shapes;
  imp_storage = fn.Imp.storage;

  named_values = Hashtbl.create 13;
  builder = Llvm.builder context;
  name = FnId.to_str fn.Imp.id;
}

module Intrinsics = struct
  let rec repeat_type (t:lltype) (n:int) =
    match n with 0 -> [] | _ -> t::(repeat_type t (n-1))

  let op_type (eltT:lltype) (arity:int) =
    Llvm.function_type eltT (Array.of_list (repeat_type eltT arity))

  let unop_type (eltT:lltype) = op_type eltT 1
  let binop_type (eltT:lltype) = op_type eltT 2

  let declare_prim name fnT  = Llvm.declare_function name fnT llvm_module

  let mk_prim name (primVariants: (ImpType.t * llvalue) list) (t:ImpType.t) =
    try List.assoc t primVariants
    with _ ->
      failwith $ Printf.sprintf
        "%s not implemented for %s"
        name
        (ImpType.to_str t)

  let sqrt = mk_prim "sqrt" [
    ImpType.float32_t, declare_prim "llvm.sqrt.f32" (op_type float32_t 1);
    ImpType.float64_t, declare_prim "llvm.sqrt.f64" (op_type float64_t 1);
    ImpType.VectorT(Type.Float32T, 4),
      declare_prim "llvm.x86.sse.sqrt.ps" (unop_type vec4_float32_t);
    ImpType.VectorT(Type.Float64T, 2),
      declare_prim "llvm.x86.sse2.sqrt.pd" (unop_type vec2_float64_t);
  ]

  (* WILL THESE EVER GET USED? *)
  let powi32 =
    declare_prim "llvm.powi.f32" (function_type float32_t [|float32_t;int32_t|])
  let powi64 =
    declare_prim "llvm.powi.f64" (function_type float64_t [|float64_t;int32_t|])

  let pow = mk_prim "pow" [
    ImpType.float32_t, declare_prim "llvm.pow.f32" (binop_type float32_t);
    ImpType.float64_t, declare_prim "llvm.pow.f64" (binop_type float64_t);
  ]

  let exp = mk_prim "exp" [
    ImpType.float32_t, declare_prim "llvm.exp.f32" (op_type float32_t 1);
    ImpType.float64_t, declare_prim "llvm.exp.f64" (op_type float64_t 1);
  ]

  let log = mk_prim "log" [
    ImpType.float32_t, declare_prim "llvm.log.f32" (op_type float32_t 1);
    ImpType.float64_t, declare_prim "llvm.log.f64" (op_type float64_t 1);
  ]

  let sin = mk_prim "sin" [
    ImpType.float32_t, declare_prim "llvm.sin.f32" (op_type float32_t 1);
    ImpType.float64_t, declare_prim "llvm.sin.f64" (op_type float64_t 1);
  ]

  let cos = mk_prim "cos" [
    ImpType.float32_t, declare_prim "llvm.cos.f32" (op_type float32_t 1);
    ImpType.float64_t, declare_prim "llvm.cos.f64" (op_type float64_t 1);
  ]

  let printf =
    let fnT = Llvm.var_arg_function_type int32_t [|char_ptr_t|] in
    declare_function "printf" fnT llvm_module
end

let llvm_printf str vals builder =
  IFDEF DEBUG THEN
    let str = Llvm.build_global_stringptr str "printfstr" builder in
    let args = Array.append [|str|] (Array.of_list vals) in
    Llvm.build_call Intrinsics.printf args "printf" builder;
  ENDIF;
  ()

module Indexing = struct

  let array_field_addr_cache : (llvalue * Imp.array_field, llvalue) Hashtbl.t =
    Hashtbl.create 127

  let get_array_field_addr (fnInfo:fn_info) (array:llvalue) field : llvalue =
    match Hashtbl.find_option array_field_addr_cache (array, field) with
      | None ->
        let resultName =
          (Llvm.value_name array) ^ "."^(Imp.array_field_to_str field)^".addr"
        in
        let indices = [|zero_i32; mk_int32 (Imp.array_field_pos field) |] in
        Llvm.build_gep array indices resultName fnInfo.builder
      | Some ptr -> ptr

  let array_field_cache : (llvalue * Imp.array_field, llvalue) Hashtbl.t =
    Hashtbl.create 127
  let get_array_field (fnInfo:fn_info) (array:llvalue) field : llvalue =
    match Hashtbl.find_option array_field_cache (array, field) with
      | None ->
        let resultName =
          Llvm.value_name array ^ "."^ Imp.array_field_to_str field
        in
        let addr = get_array_field_addr fnInfo array field in
        Llvm.build_load addr resultName fnInfo.builder
      | Some fieldPtr -> fieldPtr

  let get_array_strides_elt (fnInfo:fn_info) (array:llvalue) (idx:int) =
    let strides : llvalue  = get_array_field fnInfo array ArrayStrides in
    let stridePtr =
      if idx <> 0 then
        Llvm.build_gep strides [|mk_int32 idx|] "stride_ptr" fnInfo.builder
      else
        strides
    in
    let resultName =
      Llvm.value_name array ^ ".strides.elt" ^ string_of_int idx ^ "_"
    in
    Llvm.build_load stridePtr resultName fnInfo.builder

  let get_array_shape_elt (fnInfo:fn_info) (array:llvalue) (idx:int) =
   let shape : llvalue  = get_array_field fnInfo array ArrayShape in
   let eltPtr : llvalue =
     if idx <> 0 then
       Llvm.build_gep shape [|mk_int32 idx|] "shape_ptr" fnInfo.builder
     else
       shape
   in
   Llvm.build_load eltPtr ("dim" ^ (string_of_int idx) ^ "_") fnInfo.builder

  (* convert a list of indices into an address offset *)
  let rec compute_offset
      (fnInfo:fn_info)
      (array:Llvm.llvalue)
      ?(i=0)
      ?(offset=zero_i32)
      (indices:llvalue list) : llvalue =
    match indices with
    | currIdx :: otherIndices ->
      if Llvm.is_null currIdx then
        compute_offset fnInfo array ~i:(i+1) ~offset otherIndices
      else (
        let strideVal : llvalue = get_array_strides_elt fnInfo array i in
        let currOffset =
          Llvm.build_mul strideVal currIdx "offset_term" fnInfo.builder
        in
        (*Llvm.dump_value currOffset;*)
        let newOffset =
          if Llvm.is_null offset then currOffset
          else
            let offsetName = "offset_" ^ (string_of_int i) ^ "_" in
            Llvm.build_add offset currOffset offsetName fnInfo.builder
        in
        compute_offset fnInfo array ~i:(i+1) ~offset:newOffset otherIndices
      )
    | [] ->
      Printf.printf "Done with compute_offset\n%!";
      offset

  let compile_arr_idx
      (array:Llvm.llvalue)
      (indices:Llvm.llvalue list)
      (imp_elt_t:Type.elt_t)
      (fnInfo:fn_info) =
    IFDEF DEBUG THEN
      Printf.printf "[LLVM compile_arr_idx]: %s[%s] : %s\n"
        (Llvm.value_name array)
        (String.concat ", " (List.map Llvm.value_name indices))
        (Type.elt_to_str imp_elt_t)
    ENDIF;
    let dataPtr = get_array_field fnInfo array ArrayData  in
    (*Llvm.dump_value dataPtr;*)
     let offset = compute_offset fnInfo array indices in
    let addrName = Llvm.value_name array ^ "_idxAddr" in
    let newAddr = Llvm.build_gep dataPtr [|offset|] addrName fnInfo.builder in
    newAddr

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

  let get_array_data_ptr (fnInfo:fn_info) (array:llvalue) : llvalue =
    let dataFieldPtr =
      Llvm.build_gep array [|zero_i32; zero_i32|] "data_field" fnInfo.builder
    in
    Llvm.build_load dataFieldPtr "data_addr" fnInfo.builder

  (* convert a list of indices into an address offset *)
  let rec compute_offset
    (fnInfo:fn_info)
    (array:Llvm.llvalue)
    ?(i=0)
    ?(offset=zero_i32)
    (indices:llvalue list) : llvalue =
  match indices with
  | currIdx :: otherIndices ->
    if Llvm.is_null currIdx then
      compute_offset fnInfo array ~i:(i+1) ~offset otherIndices
    else begin
      let strideVal : llvalue = get_array_strides_elt fnInfo array i in
      let currOffset =
        Llvm.build_mul strideVal currIdx "offset_term" fnInfo.builder
      in
      let newOffset =
        if Llvm.is_null offset then currOffset
        else Llvm.build_add offset currOffset "offset" fnInfo.builder
      in
      compute_offset fnInfo array ~i:(i+1) ~offset:newOffset otherIndices
    end
  | [] -> offset

  let compile_arr_idx
      (array:Llvm.llvalue)
      (indices:Llvm.llvalue list)
      (imp_elt_t:Type.elt_t)
      (fnInfo:fn_info) =
    IFDEF DEBUG THEN
      Printf.printf "[LLVM compile_arr_idx]: %s[%s] : %s\n"
        (Llvm.value_name array)
        (String.concat ", " (List.map Llvm.value_name indices))
        (Type.elt_to_str imp_elt_t)
    ENDIF;
    let dataPtr = get_array_field fnInfo array ArrayData  in
    let offset = compute_offset fnInfo array indices in
    Llvm.build_gep dataPtr [|offset|] "idxAddr" fnInfo.builder

  let compile_range_load
      (array:Llvm.llvalue)
      (indices:Llvm.llvalue list)
      (imp_elt_t:Type.elt_t)
      (fnInfo:fn_info) =
    let startIdx = Llvm.const_int LlvmType.int32_t 0 in
    let startPtr =
      Llvm.build_gep array [|zero_i32;startIdx|] "gep_start" fnInfo.builder
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

  (* assume vector slice is through contiguous data *)
  let compile_vec_slice
      (array:Llvm.llvalue)
      (idx:Llvm.llvalue)
      (impT:ImpType.t)
      (fnInfo:fn_info) =
    match impT with
    | ImpType.VectorT(eltT, width) ->
      (* WARNING: Assume slice is through an ordinary array, not a range *)
      let dataPtr = get_array_field fnInfo array ArrayData  in
      let offsetScalarPtr =
        Llvm.build_gep dataPtr [|idx|] "vecOffset" fnInfo.builder
      in
      let llvmVecT = Llvm.pointer_type (LlvmType.of_imp_type impT) in
      Llvm.build_pointercast offsetScalarPtr llvmVecT "vecPtr" fnInfo.builder
    | _ -> failwith "[Imp_to_LLVM] Error compiling vec slice"

    let allocate_local_array_info
      ?(add_to_cache=true)
      (fnInfo:fn_info)
      (name:string)
      (impT:ImpType.t)
      (llvmT:lltype) =
      let local = Llvm.build_alloca llvmT (name^"_local") fnInfo.builder in
      let rank = mk_int32 (ImpType.rank impT) in
      let shape =
        Llvm.build_array_alloca int32_t rank (name^"_shape") fnInfo.builder
      in
      let shapeField = get_array_field_addr fnInfo local ArrayShape in
      Llvm.build_store shape shapeField fnInfo.builder ;
      let strides =
        Llvm.build_array_alloca int32_t rank (name^"_strides") fnInfo.builder
      in
      let stridesField = get_array_field_addr fnInfo local ArrayStrides in
      Llvm.build_store strides stridesField  fnInfo.builder;
      if add_to_cache then (
        Hashtbl.add array_field_addr_cache (local, ArrayShape) shapeField;
        Hashtbl.add array_field_cache (local, ArrayShape) shape;
        Hashtbl.add array_field_addr_cache (local, ArrayStrides) stridesField;
        Hashtbl.add array_field_cache (local, ArrayStrides) strides
      );
      local



  let copy_array
    (fnInfo:fn_info)
    ?exclude_dim
    ?apply_to_elts
    srcAddr
    destAddr
    nelts =
    let destIdx = ref 0 in
    for srcIdx = 0 to nelts - 1 do
      if match exclude_dim with None -> true | Some j -> srcIdx <> j then begin
        let src =
          Llvm.build_gep srcAddr [|mk_int64 srcIdx|] "src" fnInfo.builder
        in
        (*Llvm.dump_value src;*)
        let eltName =
          (Llvm.value_name srcAddr) ^ "_elt_" ^ (string_of_int srcIdx) ^ "_"
        in
        let elt =
          let rawElt = Llvm.build_load src eltName fnInfo.builder in
          match apply_to_elts with
          | None -> rawElt
          | Some f -> f rawElt
        in
        let dest =
          Llvm.build_gep destAddr [|mk_int64 !destIdx|] "dest" fnInfo.builder
        in
        (*Llvm.dump_value dest;*)
        destIdx := !destIdx + 1;
        ignore (Llvm.build_store elt dest fnInfo.builder)
        (*Llvm.dump_value storeInstr*)
      end
    done



  (* to avoid loads from shape/stride arrays in the heart of tight loops, *)
  (* we instead load all of the metadata into stack allocated arrays and*)
  (* hope the fields get turned into SSA variables (and thus optimized) *)

  (* IMPORTANT: When we copy a field, we divide it by the eltsize so that*)
  (* local strides are by number of elts, not number of bytes *)
  (* TODO: What about fixdim which are already divided? *)
  let copy_array_metadata
    (fnInfo:fn_info)
    ?(convert_strides_to_num_elts=true)
    (src:llvalue)
    (dest:llvalue)
    ?exclude_dim
    rank =
    let srcData = get_array_field fnInfo src ArrayData in
    let destDataField = get_array_field_addr fnInfo dest ArrayData in
    let _ = Llvm.build_store srcData destDataField fnInfo.builder in

    let srcShapeField = get_array_field fnInfo src ArrayShape in
    let destShapeField = get_array_field fnInfo dest ArrayShape in
    copy_array fnInfo srcShapeField destShapeField rank;

    let srcStrides = get_array_field fnInfo src ArrayStrides in
    let destStrides = get_array_field fnInfo dest ArrayStrides in
    (* local strides array should be in terms of elements, not number of bytes *)
    let div_stride (byteStride:llvalue) : llvalue =
      let eltT : lltype = Llvm.element_type (Llvm.type_of srcData) in

      (* returns elt size of array as 64-bit integer *)
      let eltSize = Llvm.size_of eltT in

      let eltSize32 = Llvm.const_intcast eltSize LlvmType.int32_t in
      let newStrideName = Llvm.value_name byteStride ^ "_as_num_elts" in
      Llvm.build_sdiv byteStride eltSize32 newStrideName fnInfo.builder
    in
    if convert_strides_to_num_elts then
      copy_array fnInfo ~apply_to_elts:div_stride srcStrides destStrides rank
    else
      copy_array fnInfo srcStrides destStrides rank


end
open Indexing

let compile_const (t:ImpType.t) (n:ParNum.t) =
  let t' : lltype = LlvmType.of_imp_type t in
  (*Printf.printf "Compile const: type = %s\n%!" (Llvm.string_of_lltype t');*)
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

let compile_cmp (t:ImpType.t) op (vals:llvalue list) builder =
  let t = ImpType.elt_type t in
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

let compile_math_op (t:ImpType.t) op (vals:llvalue list) builder =
  let eltT = ImpType.elt_type t in
  match op, vals with
  | Prim.Add, [ x; y ] ->
    if Type.elt_is_int eltT then Llvm.build_add x y "addtmp" builder
    else Llvm.build_fadd x y "addftmp" builder
  | Prim.Sub, [ x; y ] ->
    if Type.elt_is_int eltT then Llvm.build_sub x y "subtmp" builder
    else Llvm.build_fsub x y "subftmp" builder
  | Prim.Mult, [ x; y ] ->
    if Type.elt_is_int eltT then Llvm.build_mul x y "multmp" builder
    else Llvm.build_fmul x y "mulftmp" builder
  | Prim.Div, [ x; y ] ->
    if Type.elt_is_int eltT then Llvm.build_sdiv x y "sdivtmp" builder
    else Llvm.build_fdiv x y "fdivtmp" builder
  | Prim.Neg, [x] ->
    let llvm_type = LlvmType.of_elt_type eltT in
    if Type.elt_is_int eltT then
      let zero = Llvm.const_int llvm_type 0 in
      Llvm.build_sub zero x "negtmp" builder
    else
      let zero = Llvm.const_float llvm_type (-0.0) in
      Llvm.build_fsub zero x "negtmp" builder
  | Prim.Sqrt, [x] ->
    Llvm.build_call (Intrinsics.sqrt t) [|x|] "sqrt" builder
  | Prim.Exp, [x] ->
    Llvm.build_call (Intrinsics.exp t) [|x|] "exp" builder
  | Prim.Ln, [x] ->
    Llvm.build_call (Intrinsics.log t) [|x|] "log" builder
  | Prim.Pow, [ x;y ] ->
    Llvm.build_call (Intrinsics.pow t) [|x;y|] "pow" builder
  | _ ->
    failwith $ Printf.sprintf "Unsupported math op %s with %d args"
      (Prim.scalar_op_to_str op) (List.length vals)

let compile_var ?(do_load=true) fnInfo (id:ID.t) =
  let name = ID.to_str id in
  match Hashtbl.find_option fnInfo.named_values name with
  | None -> failwith $"[Imp_to_LLVM] unknown variable " ^ name
  | Some ptr ->
    if do_load then build_load ptr (name ^ "_value") fnInfo.builder
    else ptr

(* Change to function? *)
let rec compile_value ?(do_load=true) fnInfo (impVal:Imp.value_node) =
  (*
  IFDEF DEBUG THEN
    Printf.printf "[Imp_to_LLVM.compile_value] %s\n%!"
      (Imp.value_node_to_str impVal)
  ENDIF;
  *)
  match impVal.value with
  | Imp.Var id -> compile_var ~do_load fnInfo id
  | Imp.DimSize (arr, idx) ->
    let llvmArr = compile_value ~do_load:false fnInfo arr in
    let llvmIdx = compile_value fnInfo idx in
    let shape = get_array_field fnInfo llvmArr ArrayShape in
    let dimPtr = Llvm.build_gep shape [|llvmIdx|] "dim_ptr" fnInfo.builder in
    Llvm.build_load dimPtr "dim" fnInfo.builder
  | Imp.Const const -> compile_const impVal.Imp.value_type const
  | Imp.VecConst vals ->
    (*Printf.printf "Compile vec constant to llvm with type %s\n%!"
      (ImpType.to_str impVal.Imp.value_type);
    *)
    let eltT = ImpType.peel impVal.Imp.value_type in
    let vals = List.map (compile_const eltT) vals in
    let llvmVec = const_vector (Array.of_list vals) in
    (*Llvm.dump_value llvmVec;*)
    llvmVec

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
    begin match impVal.value_type with
      | ImpType.VectorT (imp_elt_t, width) ->
        assert false
      | _ ->
        begin match arr.value_type with
          | ImpType.RangeT imp_elt_t ->
            compile_range_load llvmArray llvmIndices imp_elt_t fnInfo
          | ImpType.ArrayT (imp_elt_t, imp_int) ->
            let idxAddr =
              compile_arr_idx llvmArray llvmIndices imp_elt_t fnInfo
            in
            Llvm.build_load idxAddr "ret" fnInfo.builder
        end
    end
  | Imp.VecSlice (arr, idx, width) ->
    let llvmArray = compile_value ~do_load:false fnInfo arr in
    let llvmIdx = compile_value fnInfo idx in
    let idxAddr =
      compile_vec_slice llvmArray llvmIdx impVal.value_type fnInfo
    in
    Llvm.build_load idxAddr "ret" fnInfo.builder
  | _ ->
    failwith $ Printf.sprintf
      "[Imp_to_LLVM] Value not implemented %s"
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
  (*
  IFDEF DEBUG THEN
    Printf.printf "[Imp_to_LLVM.compile_stmt] %s\n%!"
      (Imp.stmt_to_str stmt)
  ENDIF;
  *)
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

  | Imp.Set({value=Idx(arr, indices)}, rhs) ->
    let arrayPtr : Llvm.llvalue = compile_value ~do_load:false fnInfo arr in
    let indexRegisters : Llvm.llvalue list = compile_values fnInfo indices in
    let rhsVal = compile_value fnInfo rhs in
    begin match rhs.value_type with
      | ImpType.ScalarT imp_elt_t ->
        let idxAddr =
          compile_arr_idx arrayPtr indexRegisters imp_elt_t fnInfo
        in
        Llvm.build_store rhsVal idxAddr fnInfo.builder
      | ImpType.VectorT (_, _) ->
        IFDEF DEBUG THEN Printf.printf "Compiling SetIdx to VectorT\n%!" ENDIF;
        assert(false);
        (* TODO: the following lines are just to get it to compile. change *)
        let idx = List.hd indexRegisters in
        let idxAddr =
          compile_vec_slice arrayPtr idx rhs.value_type fnInfo
        in
        Llvm.build_store rhsVal idxAddr fnInfo.builder
      | other -> failwith $ Printf.sprintf
        "[Imp_to_LLVM] Unsuported set index for type %s" (ImpType.to_str other)
    end;
    currBB
  | Imp.Set ({value=VecSlice(arr, idx, width)}, rhs) ->
    let arrayPtr : Llvm.llvalue = compile_value ~do_load:false fnInfo arr in
    let indexRegister : Llvm.llvalue = compile_value fnInfo idx in
    let rhsVal = compile_value fnInfo rhs in
    IFDEF DEBUG THEN Printf.printf "Compiling SetVecSlice to LLVM\n%!" ENDIF;
    let idxAddr =
      compile_vec_slice arrayPtr indexRegister rhs.value_type fnInfo
    in
    Llvm.build_store rhsVal idxAddr fnInfo.builder;
    currBB
  | Imp.Set ({value=Var lhsId}, {value=Imp.FixDim(arr, dim, idx); value_type})->
    let llvmIdx = compile_value fnInfo idx in
    let rank = ImpType.rank value_type in
    let srcArray = compile_value ~do_load:false fnInfo arr in
    let destArray = compile_var ~do_load:false fnInfo lhsId in
    let exclude_dim = ImpHelpers.get_const_int dim in
    copy_array_metadata
      ~convert_strides_to_num_elts:false
        fnInfo srcArray destArray ~exclude_dim rank
    ;
    let strideNumElts = get_array_strides_elt fnInfo srcArray exclude_dim in
    let srcData = get_array_field fnInfo srcArray Imp.ArrayData in
    let totalIdx =
      Llvm.build_mul strideNumElts llvmIdx "sliceNumElts" fnInfo.builder
    in
    let offsetPtr =
      Llvm.build_gep srcData [|totalIdx|] "fixdimPtr" fnInfo.builder
    in
    let destDataField =
      get_array_field_addr fnInfo destArray Imp.ArrayData
    in
    let _ =
      Llvm.build_store offsetPtr destDataField fnInfo.builder
    in
    currBB

  | Imp.Set ({value = Var id}, rhs) ->
    let rhs : Llvm.llvalue = compile_value fnInfo rhs in
    begin match Hashtbl.find_option fnInfo.named_values (ID.to_str id) with
      | None -> failwith  ("unknown variable name " ^ (ID.to_str id))
      | Some register ->
        let instr = Llvm.build_store rhs register fnInfo.builder in
        currBB
    end

  | other ->
    failwith $ Printf.sprintf "[Imp_to_LLVM] Unsupported statement %s"
    (Imp.stmt_to_str other)


let rec compile_nelts_in_dim fnInfo = function
  | SymbolicShape.Const i -> mk_int32 i
  | SymbolicShape.Op (dim_op, x, y) ->
    let x' = compile_nelts_in_dim fnInfo x in
    let y' = compile_nelts_in_dim fnInfo y in
    (match dim_op with
      | SymbolicShape.Mult ->
        Llvm.build_mul x' y' "dim_mul" fnInfo.builder
      | SymbolicShape.Add ->
        Llvm.build_add x' y' "dim_add" fnInfo.builder
      | SymbolicShape.Max -> assert false
    )
  | SymbolicShape.Dim (id, axis) ->
    let arr = compile_var ~do_load:false fnInfo id in
    get_array_shape_elt fnInfo arr axis

let rec compile_nelts_in_shape fnInfo = function
  | [d] -> compile_nelts_in_dim fnInfo d
  | d :: ds ->
    let llvmDim = compile_nelts_in_dim fnInfo d in
    let llvmRest = compile_nelts_in_shape fnInfo ds in
    Llvm.build_mul llvmDim llvmRest "partial_nelts" fnInfo.builder
  | [] -> mk_int32 1

let init_local_var (fnInfo:fn_info) (id:ID.t) =
  let impT = ID.Map.find id fnInfo.imp_types in
  let shape = ID.Map.find id fnInfo.imp_shapes in
  let llvmT = LlvmType.of_imp_type impT in
  IFDEF DEBUG THEN
    Printf.printf "Initializing local %s : %s%s to have lltype %s\n%!"
      (ID.to_str id)
      (ImpType.to_str impT)
      (if SymbolicShape.is_scalar shape then ""
       else "(shape=" ^ SymbolicShape.to_str shape ^ ")")
      (Llvm.string_of_lltype llvmT)
    ;
  ENDIF;
  let varName = ID.to_str id in
  let stackVal : llvalue =
    if ImpType.is_scalar impT || ImpType.is_vector impT then
    Llvm.build_alloca llvmT varName fnInfo.builder
    else begin
      let localArray : llvalue =
        allocate_local_array_info fnInfo varName impT llvmT
      in

      if ID.Map.find id fnInfo.imp_storage = Imp.Local then (
        let nelts = compile_nelts_in_shape fnInfo shape in
        let impEltT = ImpType.elt_type impT in
        let llvmEltT = LlvmType.of_elt_type impEltT in
        let dataFieldPtr =
          Indexing.get_array_field_addr fnInfo localArray ArrayData
        in
        let data : llvalue =
          Llvm.build_array_alloca llvmEltT nelts "local_data_ptr" fnInfo.builder
        in
        let _ = Llvm.build_store data dataFieldPtr  fnInfo.builder in

        localArray
      )
      else localArray
    end
  in
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
  let stackVal =
    match List.mem id fnInfo.input_ids, ImpType.is_scalar impT with
    (* scalar input *)
    | true, true ->
      let stackVal = Llvm.build_alloca llvmT varName fnInfo.builder in
      Llvm.build_store param stackVal fnInfo.builder;
      stackVal
    (* scalar output *)
    | false, true ->
      let ptrT = Llvm.pointer_type llvmT in
      let ptrName = varName^"_scalar_ptr" in
      Llvm.build_inttoptr param ptrT ptrName fnInfo.builder
    (* input or output arrays are just int64's which can be cast into*)
    (* struct pointers. Make local copies of their metadata *)
    | _ ->
      let ptrT = Llvm.pointer_type llvmT in
      let ptrName = varName^"_ptr" in
      let paramPtr = Llvm.build_inttoptr param ptrT ptrName fnInfo.builder in
      let local =
        allocate_local_array_info fnInfo varName impT llvmT
      in
      copy_array_metadata fnInfo paramPtr local (ImpType.rank impT);
      local
  in
  Hashtbl.add fnInfo.named_values varName stackVal

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
  let llvmFn = Llvm.declare_function fnInfo.name fnT llvm_module in
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

let finalize_function fnInfo =
  Llvm.build_ret_void fnInfo.builder

let compile_fn (fn : Imp.fn) : Llvm.llvalue =
  let fnInfo = create_fn_info fn in
  let llvmFn : Llvm.llvalue = init_compiled_fn fnInfo in
  let initBasicBlock : Llvm.llbasicblock = Llvm.entry_block llvmFn in
  let _ = compile_stmt_seq fnInfo initBasicBlock fn.body in
  finalize_function fnInfo;
  llvmFn

