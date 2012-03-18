(* pp: -parser o pa_macro.cmo *)
open Base
open Adverb
open TypedSSA


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
  | []  -> 0
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
  | Adverb.AllPairs ->
    List.map (Type.increase_rank (numAxes * 2)) nestedOutputTypes
  | Adverb.Reduce -> nestedOutputTypes

let mk_adverb_exp_node
      ?(src:SrcInfo.t option)
      (info : (FnId.t, value_nodes, value_nodes) Adverb.info) =
  let numAxes = List.length info.axes in
  let nestedOutputTypes = FnManager.output_types_of_typed_fn info.adverb_fn in
  let outputTypes : Type.t list =
    adverb_output_types info.adverb numAxes nestedOutputTypes
  in
  {
    exp = Adverb info;
    exp_types = outputTypes;
    exp_src = src
  }

(* to keep stable FnId's for repeatedly generated adverbs of the same function*)
(* we cache our results*)
type fn_cache_key = (FnId.t, Type.t list, TypedSSA.value_nodes) Adverb.info

let adverb_fn_cache : (fn_cache_key, FnId.t) Hashtbl.t = Hashtbl.create 127

let mk_adverb_fn
    ?(src:SrcInfo.t option)
    (info : (FnId.t, Type.t list, TypedSSA.value_nodes) Adverb.info)
    : TypedSSA.fn =
  assert (info.init  = None);
  match Hashtbl.find_option adverb_fn_cache info with
    | Some fnId -> FnManager.get_typed_function fnId
    | None ->
      let constructor =  function
       | inputs, outputs, [] ->
          let fixed, arrays =
            List.split_nth (List.length info.fixed_args) inputs
          in
          let valueInfo = { info with
            fixed_args = fixed;
            init = None;
            array_args = arrays;
          }
          in
          [TypedSSA.set_vals outputs (mk_adverb_exp_node valueInfo)]
        | _ -> assert false
      in
      let nAxes = List.length info.axes in
      let nestedFnId = info.adverb_fn in
      let nestedOutputTypes = FnManager.output_types_of_typed_fn nestedFnId in
      let outputTypes =
        adverb_output_types info.adverb nAxes nestedOutputTypes
      in
      let newfn =
        TypedSSA.fn_builder
          ~name:(Adverb.to_str info.adverb ^ "_wrapper")
          ~input_types:(info.fixed_args @ info.array_args)
          ~output_types:outputTypes
          constructor

      in
      FnManager.add_typed ~optimize:false newfn;
      Hashtbl.replace adverb_fn_cache info (TypedSSA.fn_id newfn);
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
