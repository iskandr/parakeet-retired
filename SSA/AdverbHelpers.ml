(* pp: -parser o pa_macro.cmo *)
open Base
open Adverb
open TypedSSA
open SSA_Helpers

let const_axes valNodes =
  if List.for_all TypedSSA.is_const_int valNodes then
    List.map TypedSSA.get_const_int valNodes
  else failwith "Expected adverb axes to be constant"

let info_with_const_axes info = { info with axes = (const_axes info.axes) }

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



let mk_adverb
      ?(src:SrcInfo.t option)
      (info : (FnId.t, value_nodes, value_nodes) Adverb.info)
      (args:value_nodes) =
  let numAxes = List.length (Adverb.axes info) in
  let nestedOutputTypes =
    FnManager.output_types_of_typed_fn (Adverb.adverb_fn info)
  in
  let outputTypes : Type.t list =
    match Adverb.adverb info with
    | Adverb.Map ->
      List.map (Type.increase_rank numAxes) nestedOutputTypes
    | Adverb.AllPairs ->
      List.map (Type.increase_rank (numAxes+2)) nestedOutputTypes
    | Adverb.Scan
    | Adverb.Reduce -> nestedOutputTypes
  in
  {
    exp = Adverb(info, args);
    exp_types = outputTypes;
    exp_src = src
  }

(* to keep stable FnId's for repeatedly generated adverbs of the same function*)
(* we cache our results*)
type fn_cache_key = {
  cache_key_info : (FnId.t, Type.t list, TypedSSA.value_nodes) Adverb.info;
  cache_key_array_types :  Type.t list
}

let adverb_fn_cache : (fn_cache_key, FnId.t) Hashtbl.t = Hashtbl.create 127

let mk_adverb_fn
    ?(src:SrcInfo.t option)
    (info : (FnId.t, Type.t list, TypedSSA.value_nodes) Adverb.info)
    (array_types:Type.t list) : TypedSSA.fn =
  assert (Adverb.init info = None);
  let nestedFnId = Adverb.adverb_fn info in
  let cache_key = {
    cache_key_info = { info with adverb_fn = nestedFnId};
    cache_key_array_types = array_types;
  }
  in
  match Hashtbl.find_option adverb_fn_cache cache_key with
    | Some fnId -> FnManager.get_typed_function fnId
    | None ->
      let axes = Adverb.axes info in
      let adverb = Adverb.adverb info in
      let fixedTypes = Adverb.fixed_args info in
      let constructor = function
        | inputs, outputs, [] ->
          let fixed, arrays = List.split_nth (List.length fixedTypes) inputs in
          let info = {
            Adverb.adverb = adverb;
            adverb_fn = nestedFnId;
            axes = axes;
            fixed_args = fixed;
            init = None;
          }
          in
          let rhs = mk_adverb info inputs in
          [TypedSSA.StmtHelpers.set_vals outputs rhs]
        | _ -> assert false
      in
      let nAxes = List.length axes in
      let nestedOutputTypes = FnManager.output_types_of_typed_fn nestedFnId in
      let outputTypes = Type.increase_ranks nAxes nestedOutputTypes in
      let newfn =
        TypedSSA.fn_builder
          ~name:(Adverb.to_str adverb ^ "_wrapper")
          ~input_types:(fixedTypes @ array_types)
          ~output_types:outputTypes
          constructor
      in
      FnManager.add_typed ~optimize:false newfn;
      Hashtbl.replace adverb_fn_cache cache_key (TypedSSA.fn_id newfn);
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