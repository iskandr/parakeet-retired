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
    | None ->
      List.map SSA_Helpers.mk_int32 (List.til rank)

let infer_adverb_axes_from_args ?axes (otherArgs:value_nodes) =
  match axes with
    | Some axes -> axes
    | None ->
      let ranks =
        List.map (fun vnode -> Type.rank vnode.value_type) otherArgs
      in
      let minRank = List.min ranks in
      List.map SSA_Helpers.mk_int32 (List.til minRank)

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