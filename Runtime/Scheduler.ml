(* pp: -parser o pa_macro.cmo *)

(* TODO: Get rid of GPU/Host distinction and replace with arbitrary number*)
(* of backends/memspaces *) 

(* the cost model function expect arguments to be described by triplets  *)
(* of their type, shape, and a boolean indicating whether that argument  *)
(* is on the gpu.                                                        *)
let describe_arg v = typeof v, shapeof v, is_on_gpu v

let describe_args vs = List.map describe_arg vs


let map fn ?(axes=[0]) ~fixed args = 
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

let reduce fn ?(axes=[0]) ~fixed args =
(match 
        CostModel.reduce_cost 
            ~fnTable: P.fnTable 
            ~init: initFundef
            ~initClosureArgs: (describe_args initClosureArgs)
            ~fn: reduceFundef
            ~closureArgs: (describe_args reduceClosureArgs)
            ~initArgs: (describe_args initArgVals)
            ~args: (describe_args argVals) 
      with 
        | CostModel.GPU, _ -> 
          DataManager.enter_data_scope (); 
          let gpuResults = 
            GpuEval.reduce
              ~init: initFundef
              ~initClosureArgs: (List.map get_gpu initClosureArgs)
              ~payload: reduceFundef 
              ~payloadClosureArgs: (List.map get_gpu reduceClosureArgs)
              ~initArgs: (List.map get_gpu initArgVals)
              ~args: (List.map get_gpu argVals) 
          in 
          let interpResults = List.map add_gpu gpuResults in 
          DataManager.exit_data_scope interpResults;
          interpResults 

        | CostModel.CPU, _ -> failwith "CPU reduction not implemented"
      )
