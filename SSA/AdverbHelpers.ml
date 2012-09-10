(* pp: -parser o pa_macro.cmo *)

open Adverb
open Base
open TypedSSA

let const_axes valNodes =
  if List.for_all TypedSSA.is_const_int valNodes then
    List.map TypedSSA.get_const_int valNodes
  else failwith "Expected adverb axes to be constant"

let adverb_with_const_axes adverb = 
  { adverb with axes = const_axes adverb.axes }

let infer_adverb_axes_from_rank rank =
  List.map TypedSSA.int32 (List.til rank)

(* given a list of input array types, infer the largest number *)
(* of axes feasible to map over them-- min of all ranks except 0 *)
let rec max_num_axes_from_array_types = function
  | [] -> 0
  | [t] -> Type.rank t
  | t::ts ->
    begin match Type.rank t, max_num_axes_from_array_types ts with
      | 0, y -> y
      | x, 0 -> x
      | x, y -> min x y
    end

let infer_adverb_axes_from_types (types:Type.t list) =
  let numAxes = max_num_axes_from_array_types types in
  List.map TypedSSA.int32 (List.til numAxes)

let infer_adverb_axes_from_args ?axes (otherArgs:value_nodes) =
  match axes with
  | Some axes -> axes
  | None ->
    let argTypes = List.map (fun vNode -> vNode.value_type) otherArgs in
    let numAxes = max_num_axes_from_array_types argTypes in
    List.map TypedSSA.int32 (List.til numAxes)

let adverb_output_types adverb numAxes nestedOutputTypes =
  match adverb with
  | Adverb.Scan
  | Adverb.Map ->
    List.map (Type.increase_rank numAxes) nestedOutputTypes
  | Adverb.Reduce -> nestedOutputTypes

let mk_adverb_exp_node
      ?(src:SrcInfo.t option)
      (adverb:(FnId.t, value_nodes, value_nodes) Adverb.t) =
  let numAxes = List.length adverb.axes in
  let nestedOutputTypes = FnManager.output_types_of_typed_fn adverb.fn in
  let outputTypes : Type.t list =
    adverb_output_types adverb.adverb_type numAxes nestedOutputTypes
  in
  {
    exp = Adverb adverb;
    exp_types = outputTypes;
    exp_src = src
  }

(* to keep stable FnId's for repeatedly generated adverbs of the same function*)
(* we cache our results*)
type fn_cache_key = (FnId.t, Type.t list, TypedSSA.value_nodes) Adverb.t

let adverb_fn_cache : (fn_cache_key, FnId.t) Hashtbl.t = Hashtbl.create 127

let mk_adverb_fn
    ?(src:SrcInfo.t option)
    (adverb : (FnId.t, Type.t list, TypedSSA.value_nodes) Adverb.t)
    : TypedSSA.fn =
  
  match Hashtbl.find_option adverb_fn_cache adverb with
  | Some fnId -> FnManager.get_typed_function fnId
  | None ->
    let n_fixed = List.length adverb.fixed in 
    let n_args = List.length adverb.args in 
    let n_init = match adverb.init with 
     | None -> 0 
     | Some vs -> List.length vs  
    in 
    let constructor =  function
      | inputs, outputs, [] ->
        let fixed, rest = List.split_nth n_fixed inputs in
        let init, arrays = List.split_nth n_init rest in
        assert (List.length arrays = n_args);
        (* TODO: Does this work with Reductions/Scans which aren't given *)
        (* explicit initial values? *)   
        let adverb' = { adverb with
          fixed = fixed;
          init = if n_init > 0 then Some init else None;
          args = arrays;
        }
        in
        [TypedSSA.set_vals outputs (mk_adverb_exp_node adverb')]
      | _ -> assert false
    in
    let nAxes = List.length adverb.axes in
    let nestedFnId = adverb.fn in
    let nestedOutputTypes = 
      FnManager.output_types_of_typed_fn nestedFnId 
    in
    let outputTypes =
      adverb_output_types adverb.adverb_type nAxes nestedOutputTypes
    in
    let name =
        Printf.sprintf "%s.%s_wrapper"
          (FnId.to_str nestedFnId)
          (Adverb.adverb_type_to_str adverb.adverb_type)
    in
    let inputTypes = 
      adverb.fixed @ 
      (Option.default [] adverb.init) @ 
      adverb.args 
    in 
    let inputNames = 
      List.take (List.length inputTypes) 
       (FnManager.typed_input_names nestedFnId)
    in
    IFDEF DEBUG THEN  
      Printf.printf 
        "[AdverbHelpers.mk_adverb_fn] Making fn with inputs %s (types = %s\n%!)" 
        (String.concat ", " inputNames)
        (String.concat ", " (List.map Type.to_str inputTypes))
    ENDIF;
    let newfn =
      TypedSSA.fn_builder
        ~name
        ~input_names:inputNames 
        ~input_types:inputTypes 
        ~output_types:outputTypes
        constructor
    in
    FnManager.add_typed ~optimize:false newfn;
    Hashtbl.replace adverb_fn_cache adverb (TypedSSA.fn_id newfn);
    newfn

let rec block_has_adverb block = Block.exists stmt_has_adverb block
and stmt_has_adverb {stmt} = match stmt with
  | TypedSSA.Set(_, {exp=TypedSSA.Adverb _}) -> true
  | TypedSSA.If(_, tBlock, fBlock, _) ->
    block_has_adverb tBlock || block_has_adverb fBlock
  | TypedSSA.WhileLoop (condBlock, _, body, _) ->
    block_has_adverb condBlock || block_has_adverb body
  | _ -> false
let fn_has_adverb {body} = block_has_adverb body
