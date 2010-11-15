open Cuda

let minFreeMemory = 50000000

(* For now, making a map-specific chunking function *)
(* For now, assume (ugh) that all inputs are the same dimensionality, which *)
(* is either 1D or 2D *)
let get_map_chunks memState inputArgs outputTypes =
  let (free, total) = cuda_device_get_free_and_total_mem () in
  let nDims = DynType.nest_depth inputArgs.(0) in
  let aux arg =
    if Hashtbl.mem memState.gpu_vals arg.dataId then
      Hashtbl.find memState.gpu_vals
    else
      ()
  in
  let argsOnHost = List.map aux inputArgs in
  let aux2 curBytes (arg::rest) =
    match rest with
      | [] -> curBytes
      | _ -> aux2 (curBytes + ) rest
  in
  let totalBytes
(*

  - Get the inputs that _aren't_ on the GPU already
  - Add up their size and the size of the output
  - If that fits in available space, return the whole inputs as chunks
  - If not, 


*)
