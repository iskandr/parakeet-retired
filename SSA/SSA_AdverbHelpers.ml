(* pp: -parser o pa_macro.cmo *)
open Base
open SSA
open SSA_Helpers

let mk_adverb
      ?(src:SrcInfo.t option)
      (adverb:Prim.adverb)
      (closure:closure)
      ?(axes:value_nodes option)
      ?(init:value_nodes option)
      (args:value_nodes)
      (outputTypes:Type.t list) =
  let adverb_args =
  {
    axes = axes;
    init = init;
    args = args;
  }
  in
  {
    exp = Adverb(adverb, closure, adverb_args);
    exp_types = outputTypes;
    exp_src = src
  }

let closure_input_types closure =
  (FnManager.get_typed_function closure.closure_fn).fn_input_types

let closure_output_types closure =
  (FnManager.get_typed_function closure.closure_fn).fn_output_types

let infer_adverb_axes_from_rank ?axes rank =
  match axes with
    | Some axes -> axes
    | None -> List.map SSA_Helpers.int32 (List.til rank)

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

let infer_adverb_axes_from_types ?axes (types:Type.t list) =
  match axes with
  | Some axes -> axes
  | None ->
    let numAxes = max_num_axes_from_array_types types in
    List.map SSA_Helpers.int32 (List.til numAxes)

let infer_adverb_axes_from_args ?axes (otherArgs:value_nodes) =
  match axes with
    | Some axes -> axes
    | None ->
      let argTypes = List.map (fun vNode -> vNode.value_type) otherArgs in
      let numAxes = max_num_axes_from_array_types argTypes in
      List.map SSA_Helpers.int32 (List.til numAxes)

let mk_map ?src closure ?axes (args:value_nodes) =
  (* if axes not specified, then infer them *)
  let axes : value_node list = infer_adverb_axes_from_args ?axes args in
  let n_axes = List.length axes in
  let outputTypes : Type.t list =
    List.map (Type.increase_rank n_axes) (closure_output_types closure)
  in
  mk_adverb ?src Prim.Map closure ~axes ?init:None args outputTypes

let mk_reduce ?src closure ?axes init args =
  let axes : value_nodes = infer_adverb_axes_from_args ?axes args in
  let outTypes : Type.t list  = closure_output_types closure in
  mk_adverb ?src Prim.Reduce closure ~axes ~init args outTypes

let mk_scan ?src closure ?axes init args =
  let axes : value_nodes = infer_adverb_axes_from_args ?axes args in
  let outTypes : Type.t list = closure_output_types closure in
  mk_adverb ?src Prim.Scan closure ~axes ~init args outTypes




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

let mk_map_fn
      ?(src:SrcInfo.t option)
      ~(nested_fn:SSA.fn)
      ?(axes : SSA.value_nodes option)
      ?(fixed_types=[])
      ~(array_types: Type.t list) =
  let axes = infer_adverb_axes_from_types ?axes array_types in
  IFDEF DEBUG THEN
    Printf.printf
      "mk_map_fn] nested=%s, axes=%s, fixed=[%s], inputs=[%s]\n"
      (FnId.to_str nested_fn.fn_id)
      (SSA.value_nodes_to_str axes)
      (Type.type_list_to_str fixed_types)
      (Type.type_list_to_str array_types)
    ;
  ENDIF;
  let cache_key = {
    nested_fn_id = nested_fn.SSA.fn_id;
    adverb = Prim.Map;
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
          [outputs <--  mk_map ?src closure ~axes arrays]
        | _ -> assert false
      in
      let nAxes = List.length axes in
      let fn =
        SSA_Helpers.fn_builder
          ~name:"map_wrapper"
          ~input_types:(fixed_types @ array_types)
          ~output_types:(Type.increase_ranks nAxes nested_fn.fn_output_types)
          constructor
      in
      Hashtbl.replace adverb_fn_cache cache_key fn.fn_id;
      fn