(* pp: -parser o pa_macro.cmo *)
open Base
open SSA
open SSA_Helpers

let infer_adverb_axes_from_rank rank =
  List.map SSA_Helpers.int32 (List.til rank)

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
  List.map SSA_Helpers.int32 (List.til numAxes)

let infer_adverb_axes_from_args ?axes (otherArgs:value_nodes) =
  match axes with
  | Some axes -> axes
  | None ->
    let argTypes = List.map (fun vNode -> vNode.value_type) otherArgs in
    let numAxes = max_num_axes_from_array_types argTypes in
    List.map SSA_Helpers.int32 (List.til numAxes)


let closure_input_types closure =
  (FnManager.get_typed_function closure.closure_fn).fn_input_types

let closure_output_types closure =
  (FnManager.get_typed_function closure.closure_fn).fn_output_types

let mk_adverb
      ?(src:SrcInfo.t option)
      (adverb:Prim.adverb)
      (closure:closure)
      ?(axes:value_nodes option)
      ?(init:value_nodes option)
      (args:value_nodes) =
  let axes : SSA.value_node list = infer_adverb_axes_from_args ?axes args in
  let numAxes = List.length axes in
  let nestedOutputTypes = closure_output_types closure in
  let outputTypes : Type.t list =
    match adverb with
    | Prim.Map ->
      List.map (Type.increase_rank numAxes) nestedOutputTypes
    | Prim.AllPairs ->
      List.map (Type.increase_rank (numAxes+2)) nestedOutputTypes
    | Prim.Scan
    | Prim.Reduce -> nestedOutputTypes
  in
  let adverb_args =
  {
    axes = Some axes;
    init = init;
    args = args;
  }
  in
  {
    exp = Adverb(adverb, closure, adverb_args);
    exp_types = outputTypes;
    exp_src = src
  }

(* to keep stable FnId's for repeatedly generated adverbs of the same function*)
(* we cache our results*)
type fn_cache_key = {
  nested_fn_id : FnId.t;
  adverb : Prim.adverb;
  fixed_types : Type.t list;
  array_types : Type.t list;
  adverb_axes : SSA.value_nodes;
}

let adverb_fn_cache : (fn_cache_key, FnId.t) Hashtbl.t = Hashtbl.create 127


(* TODO: figure out what's going on with init args *)
let mk_adverb_fn
    ?(src:SrcInfo.t option)
    ~(adverb:Prim.adverb)
    ~(nested_fn:SSA.fn)
    ?(axes:SSA.value_nodes option)
    ?(init:Type.t list option)
    ?(fixed_types : Type.t list = [])
    ~(array_types:Type.t list) : SSA.fn =
  let axes = match axes with
    | Some axes -> axes
    | None -> infer_adverb_axes_from_types array_types
  in
  IFDEF DEBUG THEN
    Printf.printf
      "[mk_adverb_fn] adverb=%s, nested=%s, axes=%s, fixed=[%s], inputs=[%s]\n"
      (Prim.adverb_to_str adverb)
      (FnId.to_str nested_fn.fn_id)
      (SSA.value_nodes_to_str axes)
      (Type.type_list_to_str fixed_types)
      (Type.type_list_to_str array_types)
    ;
  ENDIF;
  let name = (Prim.adverb_to_str adverb) ^ "_wrapper" in
  let cache_key = {
    nested_fn_id = nested_fn.SSA.fn_id;
    adverb = adverb;
    fixed_types = fixed_types;
    array_types = array_types;
    adverb_axes = axes
  }
  in
  match Hashtbl.find_option adverb_fn_cache cache_key with
    | Some fnId -> FnManager.get_typed_function fnId
    | None ->
      let constructor = function
        | inputs, outputs, [] ->
          let fixed, arrays = List.split_nth (List.length fixed_types) inputs in
          let closure = SSA_Helpers.closure nested_fn fixed in
          let rhs = mk_adverb ?src adverb closure ~axes ?init inputs in
          [outputs <--rhs ]
        | _ -> assert false
      in
      let nAxes = List.length axes in
      let fn =
        SSA_Helpers.fn_builder
          ~name:name
          ~input_types:(fixed_types @ array_types)
          ~output_types:(Type.increase_ranks nAxes nested_fn.fn_output_types)
          constructor
      in
      FnManager.add_typed ~optimize:false fn;
      Hashtbl.replace adverb_fn_cache cache_key fn.fn_id;
      fn


  let rec block_has_adverb block = Block.exists stmt_has_adverb block
  and stmt_has_adverb {stmt} = match stmt with
    | SSA.Set(_, {exp=SSA.Adverb _}) -> true
    | SSA.If(_, tBlock, fBlock, _) ->
      block_has_adverb tBlock || block_has_adverb fBlock
    | SSA.WhileLoop (condBlock, _, body, _) ->
      block_has_adverb condBlock || block_has_adverb body
    | _ -> false
 let fn_has_adverb {body} = block_has_adverb block