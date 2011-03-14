(* pp: -parser o pa_macro.cmo *)

open Base 
open Printf

type t = {
  gpu_vals : (InterpVal.DataId.t, GpuVal.gpu_val) Hashtbl.t;
  host_vals : (InterpVal.DataId.t, HostVal.host_val) Hashtbl.t; 
  types : (InterpVal.DataId.t, DynType.t) Hashtbl.t; 
  shapes : (InterpVal.DataId.t, Shape.t) Hashtbl.t; 
}
let create numvars = {
  gpu_vals = Hashtbl.create (2*numvars);
  host_vals = Hashtbl.create (2*numvars); 
  types = Hashtbl.create (2*numvars); 
  shapes = Hashtbl.create (2* numvars); 
}


let add_host state hostVal = 
  let id = InterpVal.DataId.gen() in 
  Hashtbl.add state.host_vals id hostVal;
  Hashtbl.add state.types id (HostVal.get_type hostVal); 
  Hashtbl.add state.shapes id (HostVal.get_shape hostVal); 
  InterpVal.Data id 
  
let add_gpu state gpuVal = 
  let id = InterpVal.DataId.gen() in 
  Hashtbl.add state.gpu_vals id gpuVal;
  Hashtbl.add state.types id (GpuVal.get_type gpuVal); 
  Hashtbl.add state.shapes id (GpuVal.get_shape gpuVal);   
  InterpVal.Data id 


let rec get_shape memState = function  
  | InterpVal.Scalar _ -> Shape.scalar_shape 
  | InterpVal.Data id -> Hashtbl.find memState.shapes id 
  | InterpVal.Array arr -> 
      (* assume uniform arrays *)
      let eltShape = get_shape memState arr.(0) in
      Shape.append_dim (Array.length arr) eltShape 

let rec get_type memState = function 
  | InterpVal.Scalar n -> PQNum.type_of_num n 
  | InterpVal.Data id -> Hashtbl.find memState.types id
  | InterpVal.Array arr -> 
      (* for now, assume uniformity of element types *) 
      let eltT = get_type memState arr.(0) in 
      DynType.VecT eltT 

let is_on_gpu memState = function 
  | InterpVal.Data id -> Hashtbl.mem memState.gpu_vals id 
  | InterpVal.Scalar _ -> true
  (* even if all the array's elements are on the GPU, no guarantee 
     they are a contiguous chunk 
  *)
  | InterpVal.Array _ -> false

let is_on_host memState = function 
  | InterpVal.Data id -> Hashtbl.mem memState.host_vals id 
  | InterpVal.Scalar _ -> true
  | InterpVal.Array _ -> false
  
let rec sizeof memState interpVal = 
  let t = get_type memState interpVal in 
  match interpVal with  
  | InterpVal.Scalar n -> DynType.sizeof t  
  | InterpVal.Data _ -> 
      let s = get_shape memState interpVal in 
      Shape.nelts s * (DynType.sizeof (DynType.elt_type t))
  | InterpVal.Array arr ->  
      Array.fold_left (fun sum elt -> sum + sizeof memState elt) 0 arr 

let get_gpu memState = function 
  | InterpVal.Data id -> 
    if Hashtbl.mem memState.gpu_vals id then
      Hashtbl.find memState.gpu_vals id
    else (

      let hostVal = Hashtbl.find memState.host_vals id in
      IFDEF DEBUG THEN 
        Printf.printf "Sending to GPU: --%s\n" (HostVal.to_str hostVal); ENDIF; 
        let gpuVal = GpuVal.to_gpu hostVal in
        Hashtbl.replace memState.gpu_vals id gpuVal; 
        gpuVal
   )
  | InterpVal.Scalar n -> GpuVal.GpuScalar n  
  | InterpVal.Array arr ->
      (* for now, assume all rows are of uniform type/size *)
      let nrows = Array.length arr in   
      let elt = arr.(0) in 
      let eltSize = sizeof memState elt in
      let eltType = get_type memState elt in 
      let eltShape = get_shape memState elt in  
      let nbytes = nrows * eltSize in
      let finalShape = Shape.append_dim nrows eltShape in 
      let destVal = GpuVal.mk_gpu_vec (DynType.VecT eltType) finalShape in
      let destPtr = GpuVal.get_ptr destVal in 
      for i = 0 to nrows - 1 do 
        let currPtr = Int64.add destPtr (Int64.of_int $ i * eltSize) in 
        match arr.(i) with 
          | InterpVal.Scalar (PQNum.Int32 i32) -> 
              Cuda.cuda_set_gpu_int32_vec_elt currPtr 0 i32 
          | InterpVal.Scalar (PQNum.Float32 f32) -> 
              Cuda.cuda_set_gpu_float32_vec_elt currPtr 0 f32 
          | InterpVal.Data id -> 
              if Hashtbl.mem memState.gpu_vals id then 
                let eltVal = Hashtbl.find memState.gpu_vals id in 
                let eltPtr = GpuVal.get_ptr eltVal in 
                Cuda.cuda_memcpy_device_to_device currPtr eltPtr eltSize 
              else
                let eltHostVal = Hashtbl.find memState.host_vals id in 
                let eltHostPtr = HostVal.get_ptr eltHostVal in 
                Cuda.cuda_memcpy_to_device eltHostPtr currPtr eltSize  
          | _ -> assert false
       done; 
       destVal 

let rec get_host state interpVal = 
  match interpVal with  
  | InterpVal.Data id -> 
    if Hashtbl.mem state.host_vals id then
      Hashtbl.find state.host_vals id
    else (
      IFDEF DEBUG THEN 
        Printf.printf 
          "[MemoryState->get_host] Initiating transfer from GPU to host\n%!"
        ;
      ENDIF; 
      let gpuVal = Hashtbl.find state.gpu_vals id in
      let hostVal = GpuVal.from_gpu gpuVal in
      Hashtbl.replace state.host_vals id hostVal;
      IFDEF DEBUG THEN
        Printf.printf "[MemoryState->get_host] Got %s \n" 
          (HostVal.to_str hostVal);
      ENDIF;
      hostVal
    )
  | InterpVal.Scalar n -> HostVal.HostScalar n 
  | InterpVal.Array arr ->
      HostVal.HostBoxedArray (Array.map (get_host state) arr)

let get_scalar state = function 
  | InterpVal.Data id -> 
      if Hashtbl.mem state.host_vals id then 
      match Hashtbl.find state.host_vals id with 
        | HostVal.HostScalar n -> n
        | _ -> assert false 
      else ( match Hashtbl.find state.gpu_vals id with 
        | GpuVal.GpuScalar n -> n 
        | _ -> assert false
      )
 | InterpVal.Scalar n -> n 
 | InterpVal.Array _ -> failwith "An array? How did that happen?"

  (* slice on the GPU or CPU? *) 
let slice memState arr idx = match arr with 
  | InterpVal.Scalar _ -> failwith "[MemoryState] Can't slice a scalar"
  | InterpVal.Array arr -> arr.(idx)
  | InterpVal.Data id -> 
      if Hashtbl.mem memState.gpu_vals id then 
        let gpuVal = Hashtbl.find memState.gpu_vals id in 
        add_gpu memState (GpuVal.get_slice gpuVal idx) 
      else 
        let hostVal = Hashtbl.find memState.host_vals id in 
        add_host memState (HostVal.get_slice hostVal idx)   
 
let free_gpu state id =
  if Hashtbl.mem state.gpu_vals id then begin
    GpuVal.free (Hashtbl.find state.gpu_vals id);
    Hashtbl.remove state.host_vals id
  end

let free_host state id =
  if Hashtbl.mem state.host_vals id then begin
    GpuVal.free (Hashtbl.find state.gpu_vals id);
    Hashtbl.remove state.host_vals id
  end

(* free identifier on both host and gpu *) 
let free state id = free_gpu state id; free_host state id

let free_all_gpu state =
  Hashtbl.iter (fun _ gpuVal -> GpuVal.free gpuVal) state.gpu_vals;
  Hashtbl.clear state.gpu_vals
      
    
 (*
let rec host_transfer_time memState interpVal = 
  match interpVal with 
  | InterpVal.Data id -> 
      if Hashtbl.mem memState.host_vals id then 0
      else  
        (* assume 2MB / millisecond *) 
        let transferRate = 2097152 in
        let nbytes = sizeof memState interpVal in
        (* assume initiating a transfer requires at least 1 millisecond *)
        1 + nbytes / transferRate   
  | InterpVal.Scalar _ -> 0 
  | InterpVal.Array arr -> 
      Array.sum (Array.map (host_transfer_time memState) arr) 
  *)