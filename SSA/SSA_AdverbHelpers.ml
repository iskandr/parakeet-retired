open SSA
open SSA_Helpers

let mk_adverb ?src adverb closure ?(axes=[0]) ?init args outputTypes =
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


let mk_map ?src closure ?(axes=[0]) args =
  let n_axes = List.length axes in
  let outputTypes =
    List.map (Type.increase_rank n_axes) (closure_output_types closure)
  in
  mk_adverb ?src Prim.Map closure ~axes  args outputTypes

let mk_reduce ?src closure ?(axes=[0]) initArgs args =
  let outTypes = closure_output_types closure in
  mk_adverb ?src Prim.Reduce closure ~axes ~init:initArgs args outTypes

let mk_scan ?src closure ?(axes=[0]) initArgs args =
  let outTypes = closure_output_types closure in
  mk_adverb ?src Prim.Scan closure ~axes ~init:initArgs args outTypes