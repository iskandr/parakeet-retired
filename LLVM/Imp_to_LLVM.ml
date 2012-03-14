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

  let fflush =
    let fnT = Llvm.function_type int32_t [|int32_t|] in
    declare_function "fflush" fnT llvm_module
end

let debug_in_generated_code = true

let llvm_printf str vals builder =
  IFDEF DEBUG THEN
    if debug_in_generated_code then (
      let str = Llvm.build_global_stringptr str "printfstr" builder in
      let args = Array.append [|str|] (Array.of_list vals) in
      ignore (Llvm.build_call Intrinsics.printf args "printf" builder)
    )
  ENDIF;
  ()

(* insert debug printf into compiled code *)
let debug str builder =
  IFDEF DEBUG THEN
    if debug_in_generated_code then (
      llvm_printf ("\t DEBUG: " ^ str ^ "\n") [] builder;
      ignore $
        Llvm.build_call Intrinsics.fflush [|mk_int32 0|] "flush_return" builder
    )
  ENDIF;
  ()



module Indexing = struct
(*
  let cache_fn
       (f : fn_info -> 'a -> 'b) : (?add_to_cache:bool -> fn_info -> 'a -> 'b) =
    let cache : ('a, 'b) Hashtbl.t = Hashtbl.create 127 in
    fun ?(add_to_cache=false) fnInfo key ->
      match Hashtbl.find_option cache key with
        | Some found -> found
        | None ->
          let value = f fnInfo key in
          if add_to_cache then Hashtbl.add cache key value;
          value
 *)

  let array_field_addr_cache : (llvalue * Imp.array_field, llvalue) Hashtbl.t =
    Hashtbl.create 127

  let get_array_field_addr
        ?(add_to_cache=false) (fnInfo:fn_info) (array:llvalue) field : llvalue =
    let key = (array,field) in
    match Hashtbl.find_option array_field_addr_cache key with
      | None ->
        let resultName =
          (Llvm.value_name array) ^ "."^(Imp.array_field_to_str field)^".addr"
        in
        let indices = [|zero_i32; mk_int32 (Imp.array_field_pos field) |] in
        let addr = Llvm.build_gep array indices resultName fnInfo.builder in
        if add_to_cache then Hashtbl.add array_field_addr_cache key addr;
        addr
      | Some ptr -> ptr


  let array_field_cache : (llvalue * Imp.array_field, llvalue) Hashtbl.t =
    Hashtbl.create 127

  let get_array_field ?(add_to_cache=false) fnInfo array field =
    let key = (array, field) in
    match Hashtbl.find_option array_field_cache key with
      | None ->
        let resultName =
          Llvm.value_name array ^ "."^ Imp.array_field_to_str field
        in
        let addr = get_array_field_addr  ~add_to_cache fnInfo array field in
        let field = Llvm.build_load addr resultName fnInfo.builder in
        if add_to_cache then Hashtbl.add array_field_cache key field;
        field
      | Some fieldPtr -> fieldPtr


  let array_field_elt_addr_cache = Hashtbl.create 127

    let get_array_field_elt_addr ?(add_to_cache=false) fnInfo array field idx =
    let key = (array, field, idx) in
    match Hashtbl.find_option array_field_elt_addr_cache key with
      | None ->
        let fieldPtr = get_array_field fnInfo array field in
        let eltAddrName =
           Printf.sprintf "%s.%s_elt%d_addr"
             (Llvm.value_name array)
             (Imp.array_field_to_str field)
             idx
          in
        let eltAddr =
          if idx <> 0 then
            Llvm.build_gep fieldPtr [|mk_int32 idx|] eltAddrName fnInfo.builder
          else fieldPtr
        in
        if add_to_cache then Hashtbl.add array_field_elt_addr_cache key eltAddr;
        eltAddr
      | Some eltAddr -> eltAddr


  let array_field_elt_cache : (llvalue * array_field * int, llvalue) Hashtbl.t =
    Hashtbl.create 127

  let get_array_field_elt
        ?(add_to_cache=false) (fnInfo:fn_info) (array:llvalue) field idx =
    let key = (array, field, idx) in
    match Hashtbl.find_option array_field_elt_cache key with
      | None ->
        let eltAddr =
          get_array_field_elt_addr ~add_to_cache fnInfo array field idx
        in
        let eltName =
          Printf.sprintf "%s.%s_elt%d_"
            (Llvm.value_name array)
            (Imp.array_field_to_str field)
            idx
        in
        let elt = Llvm.build_load eltAddr eltName fnInfo.builder in
        if add_to_cache then Hashtbl.add array_field_elt_cache key elt;
        elt
      | Some elt -> elt

  let get_array_strides_elt (fnInfo:fn_info) (array:llvalue) (idx:int) =
    get_array_field_elt fnInfo array ArrayStrides idx

  let get_array_shape_elt (fnInfo:fn_info) (array:llvalue) (idx:int) =
    get_array_field_elt fnInfo array ArrayShape idx

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
    Llvm.build_load idxAddr "range_load_result" fnInfo.builder

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
      llvm_printf "~~~~ i=%d, idx=%d, stride=%d, offset=%d\n"
        [mk_int32 i; currIdx; strideVal; newOffset] fnInfo.builder;
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
    (*llvm_printf " --- Base ptr: %p\n" [dataPtr] fnInfo.builder;*)
    let castName = Llvm.value_name dataPtr ^ "_as_int" in
    let dataPtrAsInt =
      Llvm.build_ptrtoint dataPtr LlvmType.int64_t castName fnInfo.builder
    in
    (*llvm_printf " --- Base ptr cast: %ld\n" [dataPtrAsInt] fnInfo.builder;*)
    let offset = compute_offset fnInfo array indices in
    (*llvm_printf " --- Computed offset: %d\n" [offset] fnInfo.builder;*)
    let offsetCast =
      Llvm.build_zext_or_bitcast
        offset LlvmType.int64_t "offsetCast" fnInfo.builder
    in

    let addrIntName = Llvm.value_name array ^ "_idx_addr_as_int" in
    let newAddrInt =
      Llvm.build_add dataPtrAsInt offsetCast addrIntName fnInfo.builder
    in
    (*llvm_printf " --- New addr cast: %ld\n" [newAddrInt] fnInfo.builder;*)
    let addrName = Llvm.value_name array ^ "_idx_addr" in
    let ptrT : lltype = Llvm.type_of dataPtr in
    Llvm.build_inttoptr newAddrInt ptrT addrName fnInfo.builder

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

  let copy_array
      (fnInfo:fn_info)
      ?exclude_dim
      ?apply_to_elts
      srcAddr
      destAddr
      nelts =
    let destIdx = ref 0 in
    for srcIdx = 0 to nelts - 1 do
      debug ("copy iter " ^ string_of_int srcIdx) fnInfo.builder;
      if match exclude_dim with None -> true | Some j -> srcIdx <> j then begin
        debug ("destIdx = "^ string_of_int !destIdx) fnInfo.builder;
        let srcName = Llvm.value_name srcAddr ^ "_src" in
        let src =
          Llvm.build_gep srcAddr [|mk_int64 srcIdx|] srcName fnInfo.builder
        in
        let eltName =
          (Llvm.value_name srcAddr) ^ "_elt_" ^ (string_of_int srcIdx) ^ "_"
        in
        let elt =
          let rawElt = Llvm.build_load src eltName fnInfo.builder in
          match apply_to_elts with
          | None -> rawElt
          | Some f -> f rawElt
        in
        let destName = Llvm.value_name destAddr ^ "_dest" in
        let dest =
          Llvm.build_gep destAddr [|mk_int64 !destIdx|] destName fnInfo.builder
        in
        destIdx := !destIdx + 1;
        ignore (Llvm.build_store elt dest fnInfo.builder)
      end
    done

  let copy_array_metadata
      (fnInfo:fn_info)
      ?data_ptr
      ?exclude_dim
      (src:llvalue)
      srcT
      (dest:llvalue) =
    let destDataField = get_array_field_addr fnInfo dest ArrayData in
    let destData = match data_ptr with
      | None -> get_array_field fnInfo src ArrayData
      | Some ptr -> ptr
    in
    ignore $ Llvm.build_store destData destDataField fnInfo.builder;
    let nelts = ImpType.rank srcT in
    let srcShapeField = get_array_field fnInfo src ArrayShape in
    let destShapeField = get_array_field fnInfo dest ArrayShape in
    copy_array fnInfo ?exclude_dim srcShapeField destShapeField nelts;
    let srcStrides = get_array_field fnInfo src ArrayStrides in
    let destStrides = get_array_field fnInfo dest ArrayStrides in
    copy_array fnInfo ?exclude_dim srcStrides destStrides nelts
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
  match impVal.value with
  | Imp.Var id -> compile_var ~do_load fnInfo id
  (* optimize shape lookups to use symbolic shape information *)
  | Imp.DimSize (arr, idx) -> compile_dimsize fnInfo arr idx
  | Imp.Const const -> compile_const impVal.Imp.value_type const
  | Imp.VecConst vals ->
    let eltT = ImpType.peel impVal.Imp.value_type in
    let vals = List.map (compile_const eltT) vals in
    let llvmVec = const_vector (Array.of_list vals) in
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
    (*llvm_printf "\t array = %p\n" [llvmArray] fnInfo.builder;*)
    (*debug "getting indices" fnInfo.builder;*)
    let llvmIndices = List.map (compile_value fnInfo) indices in
    begin match impVal.value_type with
      | ImpType.VectorT (imp_elt_t, width) ->
        assert false
      | _ ->
        begin match arr.value_type with
          | ImpType.RangeT eltT ->
            compile_range_load llvmArray llvmIndices eltT fnInfo
          | ImpType.ArrayT (eltT, imp_int) ->
            let idxAddr = compile_arr_idx llvmArray llvmIndices eltT fnInfo in
            Llvm.build_load idxAddr "index_result" fnInfo.builder
          | _ -> failwith "Indexing not implemented for this array type"
        end
    end
  | Imp.VecSlice (arr, idx, width) ->
    let llvmArray = compile_value ~do_load:false fnInfo arr in
    let llvmIdx = compile_value fnInfo idx in
    let idxAddr =
      compile_vec_slice llvmArray llvmIdx impVal.value_type fnInfo
    in
    Llvm.build_load idxAddr "vec_index_result" fnInfo.builder
  | Imp.ArrayField (field, arr) ->
    let llvmArray = compile_value ~do_load:false fnInfo arr in
    get_array_field fnInfo llvmArray field
  | _ ->
    failwith $ Printf.sprintf
      "[Imp_to_LLVM] Value not implemented %s"
      (Imp.value_node_to_str impVal)

and compile_values fnInfo = function
  | [] -> []
  | vNode :: vNodes ->
    let llvmVal = compile_value fnInfo vNode in
    llvmVal :: (compile_values fnInfo vNodes)

and compile_dimsize fnInfo (arr:Imp.value_node) (idx:Imp.value_node) =
  let llvmArr = compile_value ~do_load:false fnInfo arr in
  let shape = get_array_field fnInfo llvmArr ArrayShape in
  match idx.Imp.value with
  | Imp.Const n ->
    get_array_shape_elt fnInfo llvmArr ( ParNum.to_int n)
  | _ ->
    (* if index isn't constant, compile it  *)
    let llvmIdx = compile_value fnInfo idx in
    let dimPtr = Llvm.build_gep shape [|llvmIdx|] "dim_ptr" fnInfo.builder in
    Llvm.build_load dimPtr "dim" fnInfo.builder

let compile_set fnInfo lhs rhs =
  let lhsImpT = lhs.Imp.value_type in
  let rhsImpT = rhs.Imp.value_type in
  match lhs, rhs with
    | {value=Idx(arr, indices)}, _ ->
      let arrayPtr : Llvm.llvalue = compile_value ~do_load:false fnInfo arr in
      let indexRegisters : Llvm.llvalue list = compile_values fnInfo indices in
      let rhsVal = compile_value fnInfo rhs in

      begin match rhsImpT with
        | ImpType.ScalarT imp_elt_t ->
          let idxAddr =
            compile_arr_idx arrayPtr indexRegisters imp_elt_t fnInfo
          in
          llvm_printf
            "~~~~ Storing %d at address %p\n" [rhsVal; idxAddr] fnInfo.builder;
          ignore (Llvm.build_store rhsVal idxAddr fnInfo.builder)
        | ImpType.VectorT (_, _) ->
          assert(false);
          (* TODO: the following lines are just to get it to compile. change *)
          let idx = List.hd indexRegisters in
          let idxAddr = compile_vec_slice arrayPtr idx rhsImpT fnInfo in
          ignore (Llvm.build_store rhsVal idxAddr fnInfo.builder)
        | other ->
          failwith $
            Printf.sprintf
              "[Imp_to_LLVM] Unsuported SetIndex for type %s"
              (ImpType.to_str other)
    end

  | {value=VecSlice(arr, idx, width)}, _ ->
    let arrayPtr : Llvm.llvalue = compile_value ~do_load:false fnInfo arr in
    let indexRegister : Llvm.llvalue = compile_value fnInfo idx in
    let rhsVal = compile_value fnInfo rhs in
    let idxAddr = compile_vec_slice arrayPtr indexRegister rhsImpT fnInfo in
    ignore (Llvm.build_store rhsVal idxAddr fnInfo.builder)

  | {value=Var lhsId}, {value=Imp.FixDim(arr, dim, idx); value_type} ->
    let srcArray = compile_value ~do_load:false fnInfo arr in
    let llvmIdx = compile_value fnInfo idx in
    let destArray = compile_var ~do_load:false fnInfo lhsId in
    let exclude_dim = ImpHelpers.get_const_int dim in
    let excludedStride = get_array_strides_elt fnInfo srcArray exclude_dim in
    let sliceSize =
      Llvm.build_mul excludedStride llvmIdx "sliceSize" fnInfo.builder
    in
    let eltT = ImpType.elt_type arr.Imp.value_type in
    let eltSize = mk_int32 $ Type.sizeof eltT in
    let sliceNumElts =
      Llvm.build_sdiv sliceSize eltSize "sliceNumElts" fnInfo.builder
    in
    let srcData = get_array_field fnInfo srcArray Imp.ArrayData in
    let newDataPtr =
      Llvm.build_gep srcData [|sliceNumElts|] "fixdimPtr" fnInfo.builder
    in
    copy_array_metadata
      fnInfo
      ~data_ptr:newDataPtr
      ~exclude_dim
      srcArray
      arr.value_type
      destArray

  | {value = Var id}, _  ->
    begin match Hashtbl.find_option fnInfo.named_values (ID.to_str id) with
      | None -> failwith  ("unknown variable name " ^ (ID.to_str id))
      | Some llvmLhs ->
        let llvmRhs = compile_value fnInfo rhs in
        (match lhsImpT, rhsImpT with
          | ImpType.ArrayT _, ImpType.PtrT(eltT, Some nelts) ->
            let data = get_array_field fnInfo llvmLhs Imp.ArrayData in
            (*ignore $ Llvm.build_store llvmRhs dataField fnInfo.builder;*)
            copy_array fnInfo llvmRhs data nelts;
            let strides =  get_array_field fnInfo llvmLhs Imp.ArrayStrides in
            let eltSize = Type.sizeof eltT in
            ignore $ Llvm.build_store (mk_int32 eltSize) strides fnInfo.builder;
            let shape = get_array_field fnInfo llvmLhs Imp.ArrayShape in
            ignore $ Llvm.build_store (mk_int32 nelts) shape fnInfo.builder


          | ImpType.ArrayT _, ImpType.PtrT (_, None) ->
            failwith "Assignment from unbounded pointer to array not supported"
          | _ ->
          ignore (Llvm.build_store llvmRhs llvmLhs fnInfo.builder)
        )
    end
  | _ -> failwith "[Imp_to_LLVM] Assignment not implemented"


let rec compile_stmt_seq fnInfo currBB = function
  | [] -> currBB
  | head :: tail ->
    let newBB = compile_stmt fnInfo currBB head in
    compile_stmt_seq fnInfo newBB tail

and compile_stmt fnInfo currBB stmt =
  IFDEF DEBUG THEN
    let stmtStr = Imp.stmt_to_str stmt in
    Printf.printf "[Imp_to_LLVM] Compiling statement %s\n" stmtStr;
    debug (">> " ^ stmtStr) fnInfo.builder;
  ENDIF;
  match stmt with
  | Imp.Comment _ -> currBB
  | Imp.SyncThreads _ ->
    failwith "Unexpected syncthreads statement in LLVM backend"
  | Imp.If (cond, trueBranch, falseBranch) ->
    let llCond = compile_value fnInfo cond in
    let zero = Llvm.const_int (Llvm.type_of llCond) 0 in
    let cond_val =
      Llvm.build_icmp Llvm.Icmp.Ne llCond zero "ifcond" fnInfo.builder
    in
    let fn = Llvm.block_parent currBB in
    (* Return a new basic block that's empty and positioned after the if *)
    let afterBB = Llvm.append_block context "afterif" fn in
    let trueBB = Llvm.append_block context "then" fn in
    Llvm.position_at_end trueBB fnInfo.builder;
    let _ = compile_stmt_seq fnInfo trueBB trueBranch in
    let _ = Llvm.build_br afterBB fnInfo.builder in
    let falseBB = Llvm.append_block context "else" fn in
    Llvm.position_at_end falseBB fnInfo.builder;
    let _ = compile_stmt_seq fnInfo falseBB falseBranch in
    let _ = Llvm.build_br afterBB fnInfo.builder in
    Llvm.position_at_end currBB fnInfo.builder;
    let _ = Llvm.build_cond_br cond_val trueBB falseBB fnInfo.builder in
    Llvm.position_at_end afterBB fnInfo.builder;
    afterBB

  | Imp.While (cond, body) ->
    let fn = Llvm.block_parent currBB in
    let afterBB = Llvm.append_block context "after" fn in
    let loopBB = Llvm.append_block context "loop" fn in
    let condBB = Llvm.append_block context "cond" fn in
    let _ = Llvm.build_br condBB fnInfo.builder in
    Llvm.position_at_end condBB fnInfo.builder;
    let llCond = compile_value fnInfo cond in
    let zero = Llvm.const_int (Llvm.type_of llCond) 0 in
    let cond_val =
      Llvm.build_icmp Llvm.Icmp.Ne llCond zero "whilecond" fnInfo.builder
    in
    let _ = Llvm.build_cond_br cond_val loopBB afterBB fnInfo.builder in
    Llvm.position_at_end loopBB fnInfo.builder;
    let _ = compile_stmt_seq fnInfo loopBB body in
    let _ = Llvm.build_br condBB fnInfo.builder in
    Llvm.position_at_end afterBB fnInfo.builder;
    afterBB
  | Imp.Set(lhs, rhs) -> compile_set fnInfo lhs rhs; currBB

module Init = struct
  let rec compile_dim fnInfo = function
    | SymbolicShape.Const i -> mk_int32 i
    | SymbolicShape.Op (dim_op, x, y) ->
      let x' = compile_dim fnInfo x in
      let y' = compile_dim fnInfo y in
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


  (* allocate fields without creating a struct to combine them *)
  let allocate_local_shape_and_strides fnInfo name impT =
    let rank = mk_int32 (ImpType.rank impT) in
    let shape =
      Llvm.build_array_alloca int32_t rank (name^"_shape") fnInfo.builder
    in
    let strides =
      Llvm.build_array_alloca int32_t rank (name^"_strides") fnInfo.builder
    in
    shape, strides

  let allocate_local_array_struct
      ?(add_to_cache=true)
      (fnInfo:fn_info)
      (name:string)
      (impT:ImpType.t) =
    IFDEF DEBUG THEN
      Printf.printf ""
    ENDIF;
    let shape, strides = allocate_local_shape_and_strides fnInfo name impT in

    let llvmT = LlvmType.of_imp_type impT in
    let local = Llvm.build_alloca llvmT (name^"_local") fnInfo.builder in
    let shapeField =
      get_array_field_addr ~add_to_cache fnInfo local ArrayShape
    in
    let stridesField =
      get_array_field_addr ~add_to_cache  fnInfo local ArrayStrides
    in
    let _ = Llvm.build_store strides stridesField  fnInfo.builder in
    let _ = Llvm.build_store shape shapeField fnInfo.builder  in
    if add_to_cache then (
      Hashtbl.add array_field_cache (local, ArrayShape) shape;
      Hashtbl.add array_field_cache (local, ArrayStrides) strides;
    )
    ;
    local

  let allocate_local_array fnInfo (arr:llvalue) impT (shape:SymbolicShape.t) =
    let arrName = Llvm.value_name arr in
    Printf.printf "Allocating local array %s : %s (shape=%s)\n%!"
      arrName
      (ImpType.to_str impT)
      (SymbolicShape.to_str shape)
    ;

    let rank = ImpType.rank impT in
    let shapePtr =
      Indexing.get_array_field ~add_to_cache:true fnInfo arr ArrayShape
    in
    let shapeElts = Array.of_list $ List.map (compile_dim fnInfo) shape in
    (* fill llvm shape array *)
    for i = 0 to rank -1 do
      let dimName = Printf.sprintf "shape_elt%d_ptr" i in
      let dimPtr : llvalue =
        Llvm.build_gep shapePtr [|mk_int32 i|] dimName fnInfo.builder
      in
      let shapeElt = shapeElts.(i) in
      ignore (Llvm.build_store shapeElt dimPtr fnInfo.builder);
      Hashtbl.add array_field_elt_addr_cache (arr, ArrayShape, i) dimPtr;
      Hashtbl.add array_field_elt_cache (arr, ArrayShape, i) shapeElt;
    done;
    let stridesPtr =
      Indexing.get_array_field ~add_to_cache:true fnInfo arr ArrayStrides
    in
    let impEltT = ImpType.elt_type impT in
    let eltSize : llvalue = mk_int32 $ Type.sizeof impEltT in
    let strideAcc = ref eltSize in
    (* assume row major layout, so last idx has stride 1  *)
    (* store strides as number of elements, not number of bytes *)
    for i = rank - 1 downto 0 do
      let name = Printf.sprintf "%s_stride%d_" arrName i in
      if i < rank - 2 then (
        let dim = shapeElts.(i+1) in
        strideAcc := Llvm.build_mul !strideAcc dim name fnInfo.builder
      );
      let strideEltPtr =
        Llvm.build_gep stridesPtr [|mk_int32 i|] (name ^ "ptr") fnInfo.builder
      in
      ignore $ Llvm.build_store !strideAcc strideEltPtr fnInfo.builder;
      let cacheKey = (arr, ArrayStrides, i) in
      Hashtbl.add array_field_elt_addr_cache cacheKey strideEltPtr;
      Hashtbl.add array_field_elt_cache cacheKey !strideAcc
    done;
    let nelts =
      if rank = 1 then shapeElts.(0)
      else
        let fullSizeName = arrName^"_size_in_bytes" in
        let fullSize =
          Llvm.build_mul !strideAcc shapeElts.(0) fullSizeName fnInfo.builder
        in
        let neltsName = (arrName^"_nelts") in
        Llvm.build_sdiv fullSize eltSize neltsName fnInfo.builder
    in
    let llvmEltT = LlvmType.of_elt_type impEltT in
    let dataFieldPtr =
      Indexing.get_array_field_addr ~add_to_cache:true fnInfo arr ArrayData
    in
    let data : llvalue =
      Llvm.build_array_alloca llvmEltT nelts "local_data_ptr" fnInfo.builder
    in
    ignore $ Llvm.build_store data dataFieldPtr fnInfo.builder;
    Hashtbl.add array_field_cache (arr, ArrayData) data

  let init_local_var (fnInfo:fn_info) (id:ID.t) =
    let impT = ID.Map.find id fnInfo.imp_types in
    let shape = ID.Map.find id fnInfo.imp_shapes in
    let llvmT = LlvmType.of_imp_type impT in
    IFDEF DEBUG THEN
      let initStr =
        Printf.sprintf
          "Initializing local %s : %s%s to have lltype %s\n%!"
          (ID.to_str id)
          (ImpType.to_str impT)
          (if SymbolicShape.is_scalar shape then ""
           else "(shape=" ^ SymbolicShape.to_str shape ^ ")")
          (Llvm.string_of_lltype llvmT)
      in
      debug  initStr fnInfo.builder
    ENDIF;
    let varName = ID.to_str id in
    let stackVal : llvalue =
      if ImpType.is_scalar impT || ImpType.is_vector impT then
      Llvm.build_alloca llvmT varName fnInfo.builder
      (* local array *)
      else (
        let localArray : llvalue =
          allocate_local_array_struct fnInfo varName impT
        in
        if ID.Map.find id fnInfo.imp_storage = Imp.Local then
          allocate_local_array fnInfo localArray impT shape
        ;
        localArray
      )
    in
    Hashtbl.add fnInfo.named_values varName stackVal


  let preload_array_metadata fnInfo (input:llvalue) (impT:ImpType.t) =
    match impT with
      | ImpType.ArrayT (eltT, rank) ->
        ignore $ get_array_field ~add_to_cache:true fnInfo input Imp.ArrayData;
        for i = 0 to rank - 1 do
          ignore $
            get_array_field_elt ~add_to_cache:true fnInfo input Imp.ArrayShape i
          ;
          ignore $
            get_array_field_elt
              ~add_to_cache:true fnInfo input Imp.ArrayStrides i
        done
      | ImpType.PtrT(eltT, Some len) ->
          Hashtbl.add array_field_cache (input, Imp.PtrData) input;
          Hashtbl.add array_field_cache (input, Imp.PtrLen) (mk_int32 len);
      | ImpType.ScalarT _ ->
        (* do nothing for scalars *)
        ()
      | _ -> failwith "ImpType not supported"

  let init_nonlocal_var (fnInfo:fn_info) (id:ID.t) (param:Llvm.llvalue) =
    let impT = ID.Map.find id fnInfo.imp_types in
    let llvmT = LlvmType.of_imp_type impT in
    IFDEF DEBUG THEN
      let initStr =
        Printf.sprintf "Initializing nonlocal %s : %s to have lltype %s\n%!"
          (ID.to_str id)
          (ImpType.to_str impT)
          (Llvm.string_of_lltype llvmT)
      in
      debug initStr fnInfo.builder
    ENDIF;
    let varName = ID.to_str id in
    Llvm.set_value_name varName param;
    let stackVal =
      match impT with
      | ImpType.VectorT _
      | ImpType.ScalarT _ when List.mem id fnInfo.input_ids ->
        (* scalar input *)
        let stackVal = Llvm.build_alloca llvmT varName fnInfo.builder in
        ignore $ Llvm.build_store param stackVal fnInfo.builder;
        stackVal
      | _ ->
        (* output or array input *)
        let ptrT = Llvm.pointer_type llvmT in
        let ptrName = varName^"_ptr" in
        let ptr = Llvm.build_inttoptr param ptrT ptrName fnInfo.builder in
        preload_array_metadata fnInfo ptr impT;
        ptr
    in
    Hashtbl.add fnInfo.named_values varName stackVal



  let init_compiled_fn (fnInfo:fn_info) =
    let get_imp_type id =  ID.Map.find id fnInfo.imp_types in
    let get_imp_types ids = List.map get_imp_type ids in

    let impInputTypes = get_imp_types fnInfo.input_ids in
    let impOutputTypes = get_imp_types fnInfo.output_ids in

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
end

let compile_fn (fn : Imp.fn) : Llvm.llvalue =
  let fnInfo = create_fn_info fn in
  let llvmFn : Llvm.llvalue = Init.init_compiled_fn fnInfo in
  let initBasicBlock : Llvm.llbasicblock = Llvm.entry_block llvmFn in
  let _ = compile_stmt_seq fnInfo initBasicBlock fn.body in
  ignore $ Llvm.build_ret_void fnInfo.builder;
  llvmFn
