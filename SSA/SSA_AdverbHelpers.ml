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
    | None -> List.map SSA_Helpers.mk_int32 (List.til rank)

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
    List.map SSA_Helpers.mk_int32 (List.til numAxes)

let infer_adverb_axes_from_args ?axes (otherArgs:value_nodes) =
  match axes with
    | Some axes -> axes
    | None ->
      let argTypes = List.map (fun vNode -> vNode.value_type) otherArgs in
      let numAxes = max_num_axes_from_array_types argTypes in
      List.map SSA_Helpers.mk_int32 (List.til numAxes)

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


let mk_map_fn
      ?(src:SrcInfo.t option)
      ~(nested_fn:SSA.fn)
      ?(const_axes : int list option)
      ?(axes : SSA.value_nodes option)
      ?(fixed_types=[])
      ~(array_types: Type.t list) =
  let axes : SSA.value_nodes = match const_axes with
    | Some ints ->
      if (axes <> None) then
        failwith "[mk_map_fn] Can't use both ~axes and ~const_axes"
      ;
      List.map SSA_Helpers.mk_int32 ints
    | None -> infer_adverb_axes_from_types ?axes array_types
  in
  IFDEF DEBUG THEN
    Printf.printf
      "mk_map_fn] nested=%s, axes=%s, fixed=[%s], inputs=[%s]\n"
      (FnId.to_str nested_fn.fn_id)
      (SSA.value_nodes_to_str axes)
      (Type.type_list_to_str fixed_types)
      (Type.type_list_to_str input_types)
    ;
  ENDIF;
  let numAxes = List.length axes in
  let numFixed = List.length fixed_types in
  let numArrays = List.length array_types in
  let outTypes = Type.increase_ranks numAxes nestedfn.SSA.fn_output_types in

  (*type fn = {
  body: block;
  tenv : tenv;
  input_ids:ID.t list;
  output_ids: ID.t list;
  fn_input_types : Type.t list;
  fn_output_types : Type.t list;
  fn_id : FnId.t;
}*)
  let closure = mk_closure nestedfn [] in
  (* no need to specify the implict 0..numAxes list of axes since *)
  (* mk_map will automatically create that list when an ?axes argument *)
  (* isn't given *)
  SSA_Codegen.mk_codegen_fn inputTypes outTypes $ fun codegen inputs outputs ->
    codegen#emit [outputs := (SSA_AdverbHelpers.mk_map ?src closure inputs)]