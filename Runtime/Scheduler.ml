(* pp: -parser o pa_macro.cmo *)

(* TODO: Get rid of GPU/Host distinction and replace with arbitrary number*)
(* of backends/memspaces *) 

(* the cost model function expect arguments to be described by triplets  *)
(* of their type, shape, and a boolean indicating whether that argument  *)
(* is on the gpu.                                                        *)
let describe_arg v = typeof v, shapeof v, is_on_gpu v

let describe_args vs = List.map describe_arg vs


let map fn ~fixed args = 
  match CostModel.map_cost fn (describe_args fixed) (describe_args args) with 
  | CostModel.GPU, _ ->
      let gpuClosureVals = List.map DataManager.get_gpu fixed in 
      let gpuInputVals = List.map DataManager.get_gpu argVals in 
      DataManager.enter_data_scope (); 
      let gpuResults = 
        GpuEval.map 
          ~payload: fn
          ~closureArgs: gpuClosureVals 
          ~args: gpuInputVals
      in
      let results = List.map DataManager.add_gpu gpuResults in 
      DataManager.exit_data_scope results;
      results

  | CostModel.CPU, _ -> 
      IFDEF DEBUG THEN Printf.printf "[Eval] running map on CPU\n" ENDIF;
      InterpBackend.map fn fixed args  
    