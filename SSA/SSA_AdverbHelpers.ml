(* pp: -parser o pa_macro.cmo *)
open Base
open TypedSSA
open SSA_Helpers

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
      ~(info : (FnId.t, value_nodes, value_nodes) Adverb.info)
      ~(args:value_nodes) =
  let numAxes = List.length (Adverb.axes info) in
  let nestedOutputTypes = FnManager.output_types (Adverb.adverb_fn info) in
  let outputTypes : Type.t list =
    match adverb with
    | Prim.Map ->
      List.map (Type.increase_rank numAxes) nestedOutputTypes
    | Prim.AllPairs ->
      List.map (Type.increase_rank (numAxes+2)) nestedOutputTypes
    | Prim.Scan
    | Prim.Reduce -> nestedOutputTypes
  in
  {
    exp = Adverb(info, adverb_args);
    exp_types = outputTypes;
    exp_src = src
  }

(* to keep stable FnId's for repeatedly generated adverbs of the same function*)
(* we cache our results*)
type fn_cache_key = {
  adverb_info : (FnId.t, Type.t list, TypedSSA.value_nodes) Adverb.info;
  array_types :  Type.t list
}

let adverb_fn_cache : (fn_cache_key, FnId.t) Hashtbl.t = Hashtbl.create 127


(* TODO: figure out what's going on with init args *)
let mk_adverb_fn
    ?(src:SrcInfo.t option)
    (info : (TypedSSA.fn, Type.t list, TypedSSA.value_nodes option) Adverb.info)
    (array_types:Type.t list) : TypedSSA.fn =
  let axes = match Adverb.axes info with
    | Some axes -> axes
    | None -> infer_adverb_axes_from_types array_types
  in
  let fnId = TypedSSA.fn_id (Adverb.adverb_fn info) in
  let cache_key = {
    adverb_info = { info with Adverb.axes = axes; adverb_fn = fnId}
    array_types = array_types;
  }
  in

  match Hashtbl.find_option adverb_fn_cache cache_key with
    | Some fnId -> FnManager.get_typed_function fnId
    | None ->
      let adverb = Adverb.adverb info in
      let constructor = function
        | inputs, outputs, [] ->
          let fixed, arrays = List.split_nth (List.length fixed_types) inputs in
          let info = {
            Adverb.adverb = adverb;
            adverb_fn = nested_fn;
            axes = axes;
            fixed_args = fixed;
          }
          in
          let rhs = mk_adverb info inputs in
          [TypedSSA.StmtHelpers.set_vals outputs rhs]
        | _ -> assert false
      in
      let nAxes = List.length axes in
      let fn =
        TypedSSA.fn_builder
          ~name:(Prim.adverb_to_str adverb) ^ "_wrapper"
          ~input_types:(fixed_types @ array_types)
          ~output_types:(Type.increase_ranks nAxes nested_fn.fn_output_types)
          constructor
      in
      FnManager.add_typed ~optimize:false fn;
      Hashtbl.replace adverb_fn_cache cache_key fn.fn_id;
      fn


  let rec block_has_adverb block = Block.exists stmt_has_adverb block
  and stmt_has_adverb {stmt} = match stmt with
    | TypedSSA.Set(_, {exp=TypedSSA.Adverb _}) -> true
    | TypedSSA.If(_, tBlock, fBlock, _) ->
      block_has_adverb tBlock || block_has_adverb fBlock
    | TypedSSA.WhileLoop (condBlock, _, body, _) ->
      block_has_adverb condBlock || block_has_adverb body
    | _ -> false
 let fn_has_adverb {body} = block_has_adverb block