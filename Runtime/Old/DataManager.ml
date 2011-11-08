(* pp: -parser o pa_macro.cmo *)

open Base 
open Printf

type t = { 
    data_table : (DataId.t, (MemId.t * Data.t) list) Hashtbl.t; 
}

let fresh_data_id ?(refcount=0) () =
  let id = DataId.gen() in 
  Env.add_to_data_scope id; 
  RefCounting.setref id refcount; 
  id 

let state = { data_table =  Hashtbl.create 1001 }

let associate_id_with_gpu_data 
        (memState:t) 
        (gpuVec:GpuVal.gpu_vec) 
        (dataId:DataId.t) = 
  IFDEF DEBUG THEN 
    assert (not $ Hashtbl.mem memState.gpu_data dataId); 
  ENDIF; 
  Hashtbl.replace memState.gpu_data dataId gpuVec; 
  if gpuVec.vec_slice_start = None then 
    Hashtbl.replace memState.gpu_rev_lookup gpuVec.vec_ptr dataId  

let dissociate_id_from_gpu_data 
        (memState:t) 
        (gpuVec:GpuVal.gpu_vec) 
        (dataId:DataId.t) = 
  IFDEF DEBUG THEN 
    assert (Hashtbl.mem memState.gpu_data dataId); 
  ENDIF; 
  Hashtbl.remove memState.gpu_data dataId; 
  Hashtbl.remove memState.gpu_rev_lookup gpuVec.vec_ptr

  
let associate_id_with_host_data 
        (memState:t) 
        (hostVec:HostVal.host_array) 
        (dataId:DataId.t) =
  IFDEF DEBUG THEN 
    assert (not $ Hashtbl.mem memState.host_data dataId); 
  ENDIF; 
  Hashtbl.replace memState.host_data dataId hostVec; 
  if hostVec.slice_start = None then 
    Hashtbl.replace memState.host_rev_lookup hostVec.ptr dataId  

let dissociate_id_from_host_data 
        (memState:t) 
        (hostVec:HostVal.host_array) 
        (dataId:DataId.t) =
  IFDEF DEBUG THEN 
    assert (Hashtbl.mem memState.host_data dataId); 
  ENDIF;  
  Hashtbl.remove memState.host_data dataId; 
  Hashtbl.remove memState.host_rev_lookup hostVec.ptr



module ManagedAlloc = struct 

  let rec alloc_gpu memState nbytes = 
    try Alloc.smart_alloc memState.gpu_mem nbytes 
    with _ -> (
      flush_gpu_to_host memState;  
      Alloc.smart_alloc memState.gpu_mem nbytes
    )
         
  and alloc_host memState nbytes = Alloc.smart_alloc memState.host_mem nbytes 
  and vec_from_gpu memState gpuVec = 
    let dataHostPtr = alloc_host memState gpuVec.vec_nbytes in  
    Cuda.cuda_memcpy_to_host dataHostPtr gpuVec.vec_ptr gpuVec.vec_nbytes; 
    let hostVec = { 
      ptr = dataHostPtr; 
      nbytes = gpuVec.vec_nbytes;
      host_t = gpuVec.vec_t; 
      shape = gpuVec.vec_shape; 
      slice_start = None; 
    }
    in 
    IFDEF DEBUG THEN 
      Printf.printf "[MemState] vector returned from GPU: %s\n"
        (HostVal.host_vec_to_str hostVec); 
    ENDIF;
    hostVec
  and flush_gpu_to_host memState = 
    Hashtbl.iter 
      (fun dataId gpuVec -> 
        if not $ Hashtbl.mem memState.host_data dataId then
          Hashtbl.add memState.host_data dataId (vec_from_gpu memState gpuVec)
        ;  
        Alloc.delete_gpu_vec memState.gpu_mem gpuVec
      ) 
      memState.gpu_data
    ; 
    Hashtbl.clear memState.gpu_data; 
    Hashtbl.clear memState.gpu_rev_lookup
  
  let flush_gpu memState = 
    Hashtbl.iter 
      (fun _ gpuVec -> Alloc.delete_gpu_vec memState.gpu_mem gpuVec) 
      memState.gpu_data; 
    Hashtbl.clear memState.gpu_data; 
    Hashtbl.clear memState.gpu_rev_lookup


  (* send a shape vector the gpu *) 
  let shape_to_gpu memState shape =
    let shapeBytes = Shape.nbytes shape in
    IFDEF DEBUG THEN 
      Printf.printf "Sending shape to GPU: %s (%d bytes)\n"
        (Shape.to_str shape)
        shapeBytes
    ENDIF;
    let shapeDevPtr = alloc_gpu memState shapeBytes in
    let shapeHostPtr = Alloc.get_array1_ptr $ Shape.to_c_array shape in
    Cuda.cuda_memcpy_to_device shapeHostPtr shapeDevPtr shapeBytes;
    shapeDevPtr, shapeBytes
    
  let vec_to_gpu memState hostVec = 
    let gpuPtr = alloc_gpu memState hostVec.nbytes in  
    Cuda.cuda_memcpy_to_device hostVec.ptr gpuPtr hostVec.nbytes;
    let shapeDevPtr, shapeBytes = shape_to_gpu memState hostVec.shape in
    let gpuVec = {
      vec_ptr = gpuPtr; 
      vec_nbytes = hostVec.nbytes;
      vec_len= Shape.nelts hostVec.shape; 
      
      vec_shape_ptr = shapeDevPtr;
      vec_shape_nbytes = shapeBytes; 
      
      vec_shape = hostVec.shape;
      vec_t = hostVec.host_t;
       
      vec_slice_start = None;
      vec_col_major = None; 
    }
    in
    IFDEF DEBUG THEN
      Printf.printf "[MemState] vector sent to GPU: %s\n"
        (GpuVal.gpu_vec_to_str gpuVec);
    ENDIF;
    gpuVec 

  let calc_nbytes len ty shape =
    let eltT = Type.elt_type ty in
    let eltSize = Type.sizeof eltT in
    len * eltSize 


  let mk_gpu_vec memState ?refcount ?nbytes t shape =
    let nelts = Shape.nelts shape in 
    let dataBytes = 
      match nbytes with None -> calc_nbytes nelts t shape | Some sz -> sz 
    in
    let dataPtr = alloc_gpu memState dataBytes in 
    let shapePtr, shapeBytes = shape_to_gpu memState shape in 
    let gpuVec = {
      vec_ptr = dataPtr;
      vec_nbytes = dataBytes;
      vec_len = nelts;

      vec_shape_ptr = shapePtr;
      vec_shape_nbytes = shapeBytes;

      vec_shape = shape;
      vec_t = t;
      vec_slice_start = None;
      
      vec_col_major = None; 
    }
    in 
    let id = fresh_data_id ?refcount memState in 
    associate_id_with_gpu_data memState gpuVec id;  
    IFDEF DEBUG THEN
      Printf.printf "[MemState] Created %s\n%!"  (GpuVal.gpu_vec_to_str gpuVec);
    ENDIF;  
    gpuVec   

  let mk_gpu_val memState t shape = 
    IFDEF DEBUG THEN assert (Type.is_vec t); ENDIF; 
    let gpuVec = mk_gpu_vec memState t shape in 
    GpuVal.GpuArray gpuVec   

  let mk_host_vec memState ?refcount ?nbytes ty shape =
    let len = Shape.nelts shape in  
    let nbytes = match nbytes with 
      | None ->
         let eltT = Type.elt_type ty in 
         let eltSize = Type.sizeof eltT in
         len * eltSize 
      | Some n -> n
    in  
    let ptr = alloc_host memState nbytes in 
    let hostVec = {
      ptr = ptr; 
      nbytes = nbytes; 
      host_t = ty; 
      shape = shape;
      slice_start = None  
    } 
    in
    let id = fresh_data_id ?refcount memState in 
    associate_id_with_host_data memState hostVec id; 
    IFDEF DEBUG THEN
      Printf.printf "[MemState] Created %s\n%!"  (HostVal.host_vec_to_str hostVec); 
    ENDIF;  
    hostVec
    
  
end
include ManagedAlloc


let get_host_vec_id memState hostVec = 
  (* make sure we're not already tracking this pointer *)  
  if hostVec.slice_start = None && 
     Hashtbl.mem memState.host_rev_lookup hostVec.ptr then 
       Hashtbl.find memState.host_rev_lookup hostVec.ptr
  else (  
    let id = fresh_data_id memState in 
    associate_id_with_host_data memState hostVec id; 
    id 
  )
  
let rec add_host memState = function 
  | HostVal.HostScalar n -> Value.Scalar n 
  | HostVal.HostArray  hostVec -> 
      Value.Data (get_host_vec_id memState hostVec)
  | HostVal.HostBoxedArray boxedArray -> 
      let interpValArray = Array.map (add_host memState) boxedArray in 
      Value.Array interpValArray 
  
let get_gpu_vec_id memState gpuVec : DataId.t =
  (* check whether we're already tracking this memory address *) 
  if gpuVec.vec_slice_start = None && 
     Hashtbl.mem memState.gpu_rev_lookup gpuVec.vec_ptr then 
      Hashtbl.find memState.gpu_rev_lookup gpuVec.vec_ptr  
  else (
    let id = fresh_data_id memState in 
    associate_id_with_gpu_data memState gpuVec id; 
    id  
  )
  
let add_gpu memState = function 
  | GpuVal.GpuArray gpuVec -> Value.Data (get_gpu_vec_id memState gpuVec)   
  | GpuVal.GpuScalar n -> Value.Scalar n  


let is_on_gpu memState = function 
  | Value.Data id -> Hashtbl.mem memState.gpu_data id 
  | Value.Scalar _ -> true
  (* even if all the array's elements are on the GPU, no guarantee 
     they are a contiguous chunk 
  *)
  | Value.Array _ -> false

let is_on_host memState = function 
  | Value.Data id -> Hashtbl.mem memState.host_data id 
  | Value.Scalar _ -> true
  | Value.Array _ -> false
  

let get_scalar state = function 
 | Value.Scalar n -> n
 | Value.Data id -> assert false   
 | Value.Array _ -> failwith "An array? How did that happen?"

let rec get_shape memState interpVal = match interpVal with   
  | Value.Scalar _ -> Shape.scalar_shape 
  | Value.Data id ->   
      if is_on_gpu memState interpVal then
        let gpuVec = Hashtbl.find memState.gpu_data id in
        gpuVec.vec_shape 
      else (
        IFDEF DEBUG THEN 
          assert (is_on_host memState interpVal);
         ENDIF; 
        let hostVec = Hashtbl.find memState.host_data id in
        hostVec.shape  
      )
  | Value.Array arr -> 
      (* assume uniform arrays *)
      let eltShape = get_shape memState arr.(0) in
      Shape.append_dim (Array.length arr) eltShape 

let rec get_type memState interpVal = 
  match interpVal with  
  | Value.Scalar n -> ParNum.type_of_num n 
  | Value.Data id -> 
      if is_on_gpu memState interpVal then
        let gpuVec = Hashtbl.find memState.gpu_data id in  
        gpuVec.vec_t   
      else (
        assert (is_on_host memState interpVal);
        let hostVec = Hashtbl.find memState.host_data id in
        hostVec.host_t  
      )
  | Value.Array arr -> 
      (* for now, assume uniformity of element types *) 
      let eltT = get_type memState arr.(0) in 
      Type.VecT eltT 


let rec sizeof memState interpVal = 
  let t = get_type memState interpVal in 
  match interpVal with  
  | Value.Scalar n -> Type.sizeof t  
  | Value.Data _ -> 
      let s = get_shape memState interpVal in 
      Shape.nelts s * (Type.sizeof (Type.elt_type t))
  | Value.Array arr ->  
      Array.fold_left (fun sum elt -> sum + sizeof memState elt) 0 arr 

let rec get_gpu memState = function 
  | Value.Data id -> 
    if Hashtbl.mem memState.gpu_data id then
      GpuVal.GpuArray (Hashtbl.find memState.gpu_data id)
    else (
      let hostVec = Hashtbl.find memState.host_data id in
      IFDEF DEBUG THEN 
        Printf.printf "[MemState] Sending to GPU: --%s\n" 
          (HostVal.host_vec_to_str hostVec);
       ENDIF; 
      let gpuVec = vec_to_gpu memState hostVec in 
      associate_id_with_gpu_data memState gpuVec id;  
      GpuVal.GpuArray gpuVec
   )
  | Value.Scalar n -> GpuVal.GpuScalar n
  (* WARNING: This is essentially a memory leak, since we leave 
     no data id associated with the gpu memory allocated here 
   *)   
  | Value.Array arr ->
      (* for now, assume all rows are of uniform type/size *)
      let nrows = Array.length arr in   
      let elt = arr.(0) in 
      let eltSize = sizeof memState elt in
      let eltType = get_type memState elt in 
      let eltShape = get_shape memState elt in  
      let nbytes = nrows * eltSize in
      let finalType = Type.VecT eltType in 
      let finalShape = Shape.append_dim nrows eltShape in 
      let destVec = mk_gpu_vec memState finalType finalShape in
      let destPtr = destVec.vec_ptr in 
      IFDEF DEBUG THEN 
        Printf.printf "[MemState] Transferring interpreter array to GPU\n";
        Printf.printf "[MemState] -- elts = %s \n" 
            (String.concat ", " (List.map Value.to_str (Array.to_list arr)))
        ;  
        Printf.printf 
          "[MemState] -- elt size: %d, elt type: %s, elt shape: %s\n" 
            eltSize
            (Type.to_str eltType)
            (Shape.to_str eltShape)
        ;
        Printf.printf 
          "[MemState] -- total size: %d, final type : %s,  final shape: %s\n"
          nbytes
          (Type.to_str finalType)
          (Shape.to_str finalShape)
        ;  
      ENDIF; 
      for i = 0 to nrows - 1 do 
        let currPtr = Int64.add destPtr (Int64.of_int $ i * eltSize) in 
        match arr.(i) with 
          | Value.Scalar (ParNum.Int32 i32) -> 
              Cuda.cuda_set_gpu_int32_vec_elt currPtr 0 i32 
          | Value.Scalar (ParNum.Float32 f32) -> 
              Cuda.cuda_set_gpu_float32_vec_elt currPtr 0 f32 
          | Value.Data id -> 
              if Hashtbl.mem memState.gpu_data id then 
                let eltVec = Hashtbl.find memState.gpu_data id in 
                IFDEF DEBUG THEN 
                  Printf.printf 
                    "[MemState] copying array elt to gpu address %Lx: %s\n"
                    currPtr
                    (GpuVal.gpu_vec_to_str eltVec)
                ENDIF; 
                Cuda.cuda_memcpy_device_to_device eltVec.vec_ptr currPtr eltSize 
              else
                let eltHostVec = Hashtbl.find memState.host_data id in
                IFDEF DEBUG THEN 
                  Printf.printf 
                    "[MemState] copying array elt to gpu address %Lx: %s\n"
                    currPtr
                    (HostVal.host_vec_to_str eltHostVec)
                ENDIF;  
                Cuda.cuda_memcpy_to_device eltHostVec.ptr currPtr eltSize  
          | _ -> assert false 
       done; 
       GpuVal.GpuArray destVec

let rec get_host memState interpVal = 
  match interpVal with  
  | Value.Data id -> 
    if Hashtbl.mem memState.host_data id then
      HostVal.HostArray (Hashtbl.find memState.host_data id)
    else (
      IFDEF DEBUG THEN 
        Printf.printf 
          "[MemState->get_host] Initiating transfer from GPU to host\n%!"
        ;
      ENDIF; 
      let gpuVec = Hashtbl.find memState.gpu_data id in
      let hostVec = vec_from_gpu memState gpuVec in
      associate_id_with_host_data memState hostVec id; 
      IFDEF DEBUG THEN
        Printf.printf "[MemState->get_host] Got %s \n"
          (HostVal.host_vec_to_str hostVec);  
      ENDIF;
      HostVal.HostArray hostVec
    )
  | Value.Scalar n -> HostVal.HostScalar n 
  | Value.Array arr ->
      HostVal.HostBoxedArray (Array.map (get_host memState) arr)

(* assumes dataId is an array, get the GPU vec descriptor 
   (copy to gpu if necessary)
 *)
let get_gpu_vec memState dataId =
  if Hashtbl.mem memState.gpu_data dataId then
    Hashtbl.find memState.gpu_data dataId
  else (
    let hostVec = Hashtbl.find memState.host_data dataId in
    IFDEF DEBUG THEN 
      Printf.printf "[MemState] Sending to GPU: --%s\n" 
        (HostVal.host_vec_to_str hostVec);
     ENDIF; 
    let gpuVec = vec_to_gpu memState hostVec in 
    associate_id_with_gpu_data memState gpuVec dataId;  
    gpuVec 
  )
module Slicing = struct 

  (* returns a gpu_val, not a gpu_vec since the elements might be scalars, 
     also increment the ref count of the thing we're slicing into 
   *)
  let slice_gpu_vec memState gpuVec idx : gpu_val = 
    let sliceShape = Shape.slice_shape gpuVec.vec_shape [0] in
    let sliceType = Type.peel_vec gpuVec.vec_t in
    let sliceVal = 
      if Type.is_scalar sliceType then index gpuVec idx 
      else (
        (* if the slice result is not a scalar, we have to increment 
           the reference count of the original data so we don't 
           leave this slice dangling by deleting its parent 
         *) 
        inc_data_ref memState (get_gpu_vec_id memState gpuVec); 
        let bytesPerElt = Type.sizeof (Type.elt_type sliceType) in
        let nelts =  Shape.nelts sliceShape in 
        let sliceBytes = bytesPerElt * nelts in
        GpuVal.GpuArray { 
          vec_ptr = Int64.add gpuVec.vec_ptr (Int64.of_int (sliceBytes * idx));  
          vec_nbytes = sliceBytes; 
          vec_len = nelts; 
          vec_shape_ptr = Int64.add gpuVec.vec_shape_ptr (Int64.of_int 4); 
          vec_shape_nbytes = gpuVec.vec_shape_nbytes - 4; 
          vec_shape = sliceShape; 
          vec_t = sliceType;
          vec_slice_start = Some gpuVec.vec_ptr;
          (* weird that slicing is only in row major *) 
          vec_col_major = None; 
        }
      )
    in  
    IFDEF DEBUG THEN 
    Printf.printf 
      "[MemState] Got GPU slice index %d of %s\n" 
      idx
      (gpu_vec_to_str gpuVec)
    ; 
    Printf.printf "[MemState] GPU slice result: %s\n" (to_str sliceVal); 
    ENDIF; 
    sliceVal   

let slice_gpu_val (memState:t) (gpuVal:GpuVal.gpu_val) (idx:int) : gpu_val = 
  match gpuVal with   
  | GpuScalar _ -> failwith "can't slice a GPU scalar"
  | GpuArray gpuVec -> slice_gpu_vec memState gpuVec idx


  (* slice on the GPU or CPU? *) 
  let slice memState arr idx = match arr with 
    | Value.Scalar _ -> failwith "[MemState] Can't slice a scalar"
    | Value.Array arr -> arr.(idx)
    | Value.Data id ->
      (* always move data to the GPU-- should be replaced with a 
         proper array slice proxy object! 
       *)
      let gpuVec = get_gpu_vec memState id in 
      let slice = slice_gpu_vec memState gpuVec idx in  
      add_gpu memState slice 
            

  (* set either an element or a row-major slice of an array to an interpVal 
     assumed to be of the same type as other elements 
   *) 
  let rec set_gpu_idx memState (destVec:GpuVal.gpu_vec)  (idx:int) interpVal =
    (* assume uniform array *)  
    let eltSize = sizeof memState interpVal in 
    let currPtr = 
      Int64.add destVec.GpuVal.vec_ptr (Int64.of_int $ idx * eltSize) 
    in 
    match interpVal with 
    | Value.Scalar (PQNum.Int32 i32) ->  
      Cuda.cuda_set_gpu_int32_vec_elt currPtr 0 i32 
    | Value.Scalar (PQNum.Float32 f32) -> 
      Cuda.cuda_set_gpu_float32_vec_elt currPtr 0 f32 
    | Value.Data id -> 
      if Hashtbl.mem memState.gpu_data id then 
        let eltVec = Hashtbl.find memState.gpu_data id in 
        IFDEF DEBUG THEN 
          Printf.printf  "[MemState] copying array elt to gpu address %Lx: %s\n"
            currPtr (GpuVal.gpu_vec_to_str eltVec)
        ENDIF; 
        Cuda.cuda_memcpy_device_to_device eltVec.vec_ptr currPtr eltSize 
      else (
        let eltHostVec = Hashtbl.find memState.host_data id in
        IFDEF DEBUG THEN 
          Printf.printf  "[MemState] copying array elt to gpu address %Lx: %s\n"
            currPtr (HostVal.host_vec_to_str eltHostVec)
        ENDIF;  
        Cuda.cuda_memcpy_to_device eltHostVec.ptr currPtr eltSize
      )
    (* TODO: use flatten_gpu here *)   
    | interpVal -> failwith $ 
       Printf.sprintf 
         "[MemState] Can't set gpu element to host value: %s"
         (Value.to_str interpVal)
  and flatten_gpu memState arr =
    (* for now, assume all rows are of uniform type/size *)
    let nrows = Array.length arr in   
    let elt = arr.(0) in
    let eltType = get_type memState elt in
    let eltShape = get_shape memState elt in  
    let finalType = DynType.VecT eltType in
    let finalShape = Shape.append_dim nrows eltShape in 
    let destVec = mk_gpu_vec memState finalType finalShape in
    IFDEF DEBUG THEN 
      Printf.printf "[MemState] Transferring interpreter array to GPU\n";
      Printf.printf "[MemState] -- elts = %s \n" 
        (String.concat ", " (List.map Value.to_str (Array.to_list arr)));  
      Printf.printf 
        "[MemState] elt type: %s, elt shape: %s\n" 
        (DynType.to_str eltType) (Shape.to_str eltShape);
      Printf.printf 
        "[MemState] -- final type : %s,  final shape: %s\n"
        (DynType.to_str finalType) (Shape.to_str finalShape);  
    ENDIF; 
    for idx = 0 to nrows - 1 do
      set_gpu_idx memState destVec  idx arr.(idx)
    done;     
    destVec   
end

include Slicing 

let rec free_gpu_data memState gpuVec =
  match gpuVec.vec_slice_start with
  | None ->
      let nbytes = gpuVec.vec_nbytes in
      let shapeBytes = gpuVec.vec_shape_nbytes in
      let ptr = gpuVec.vec_ptr in
      let shapePtr = gpuVec.vec_shape_ptr in
      if nbytes > 0 then (
        Alloc.add_to_free_ptrs memState.gpu_mem nbytes ptr;
        if shapeBytes > 0 then
          Alloc.add_to_free_ptrs memState.gpu_mem shapeBytes shapePtr
      )
  | Some parentPtr ->
      let parentDataId = Hashtbl.find memState.gpu_rev_lookup parentPtr in
      dec_data_ref memState parentDataId

and free_host_data memState hostVec =
  match hostVec.slice_start with
  | None ->
      if hostVec.nbytes > 0 then
        Alloc.add_to_free_ptrs memState.host_mem hostVec.nbytes hostVec.ptr
  | Some parentPtr ->
      let parentDataId = Hashtbl.find memState.host_rev_lookup parentPtr in
      dec_data_ref memState parentDataId
and free_data memState dataId =
  if Hashtbl.mem memState.gpu_data dataId then (
    let gpuVec = Hashtbl.find memState.gpu_data dataId in
    free_gpu_data memState gpuVec;
    dissociate_id_from_gpu_data memState gpuVec dataId
    
  );
  if Hashtbl.mem memState.host_data dataId then (
    let hostVec = Hashtbl.find memState.host_data dataId in
    free_host_data memState hostVec;
    dissociate_id_from_host_data memState hostVec dataId
  );
  let currDataScope = Stack.pop memState.data_scopes in
  Stack.push (DataId.Set.remove dataId currDataScope) memState.data_scopes



let rec get_gpu memState = function 
  | Value.Data id ->
      let gpuVec = get_gpu_vec memState id in 
      GpuVal.GpuArray gpuVec  
   
  | Value.Scalar n -> GpuVal.GpuScalar n
  | Value.Array arr ->
     (* WARNING: This is essentially a memory leak, since we leave 
        no data id associated with the gpu memory allocated here 
      *)   
      let destVec = flatten_gpu memState arr in 
      GpuVal.GpuArray destVec
  
  
let rec get_host memState interpVal = 
  match interpVal with  
  | Value.Data id -> 
    if Hashtbl.mem memState.host_data id then
      HostVal.HostArray (Hashtbl.find memState.host_data id)
    else (
      IFDEF DEBUG THEN 
        Printf.printf 
          "[MemState->get_host] Initiating transfer from GPU to host\n%!"
        ;
      ENDIF; 
      let gpuVec = Hashtbl.find memState.gpu_data id in
      let hostVec = vec_from_gpu memState gpuVec in
      associate_id_with_host_data memState hostVec id; 
      IFDEF DEBUG THEN
        Printf.printf "[MemState->get_host] Got %s \n"
          (HostVal.host_vec_to_str hostVec);  
      ENDIF;
      HostVal.HostArray hostVec
    )
  | Value.Scalar n -> HostVal.HostScalar n 
  | Value.Array arr ->
      HostVal.HostBoxedArray (Array.map (get_host memState) arr)
