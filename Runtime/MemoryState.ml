(* pp: -parser o pa_macro.cmo *)

open Base 
open Printf
open InterpVal
open HostVal  
open GpuVal 

type t = {
  envs :  InterpVal.t ID.Map.t Stack.t; 
  data_scopes : DataId.Set.t Stack.t; 
  refcounts : (DataId.t, int) Hashtbl.t;
   
  gpu_data : (DataId.t, GpuVal.gpu_vec) Hashtbl.t;
  host_data : (DataId.t, HostVal.host_array) Hashtbl.t; 
  
  gpu_mem : Cuda.GpuPtr.t Alloc.memspace; 
  host_mem : Cuda.HostPtr.t Alloc.memspace; 
  
  (* slices requires reference counting directly on the pointers, 
     rather than the DataId.t 
   *)  
  gpu_rev_lookup : (Cuda.GpuPtr.t, DataId.t) Hashtbl.t; 
  host_rev_lookup : (Cuda.HostPtr.t, DataId.t) Hashtbl.t;  
}

let create () = 
  let memState = {
    envs = Stack.create();
    data_scopes = Stack.create ();
    refcounts = Hashtbl.create 1001; 
    gpu_data = Hashtbl.create 1001; 
    host_data = Hashtbl.create 1001; 
    gpu_rev_lookup = Hashtbl.create 1001; 
    host_rev_lookup = Hashtbl.create 1001; 
    
    gpu_mem = Alloc.create_memspace "GPU" Cuda.cuda_malloc Cuda.cuda_free;   
    host_mem = Alloc.create_memspace "Host"Alloc.c_malloc Alloc.c_free;   
  }
  in 
  (* push an initial dataScope so that we can add data *)  
  Stack.push DataId.Set.empty memState.data_scopes; 
  memState  



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


module RefCounting = struct
  
  let get_refcount memState dataId =
     Hashtbl.find memState.refcounts dataId  
    
  let set_refcount memState dataId count =
     Hashtbl.replace memState.refcounts dataId count 
  
  let clear_refcount memState dataId = 
    Hashtbl.remove memState.refcounts dataId
  
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
  
  and inc_data_ref memState (dataId: DataId.t) : unit = 
    let nrefs = get_refcount memState dataId in 
    set_refcount memState dataId (nrefs+1)
        

  and dec_data_ref memState (dataId: DataId.t) = 
    let nrefs = get_refcount memState dataId in  
    if nrefs <= 1 then (
      IFDEF DEBUG THEN 
        Printf.printf "[RefCount] Data %s reclaimed\n" (ID.to_str dataId); 
      ENDIF; 
      free_data memState dataId;                
      clear_refcount memState dataId
    ) 
    else set_refcount memState dataId (nrefs - 1)

  let rec inc_ref memState = function 
    | InterpVal.Data id -> inc_data_ref memState id
    | InterpVal.Scalar _ -> ()
    | InterpVal.Array arr -> Array.iter (inc_ref memState) arr

  let rec dec_ref memState = function 
    | InterpVal.Data id -> dec_data_ref memState id
    | InterpVal.Scalar _ -> ()
    | InterpVal.Array arr -> Array.iter (dec_ref memState) arr

end 
include RefCounting 


let fresh_data_id ?(refcount=0) memState =
  IFDEF DEBUG THEN 
    assert (not $ Stack.is_empty memState.data_scopes); 
  ENDIF; 
  let currDataScope = Stack.pop memState.data_scopes in 
  let id = DataId.gen() in 
  Stack.push (DataId.Set.add id currDataScope) memState.data_scopes;
  set_refcount memState id refcount;  
  id 

module ManagedAlloc = struct 
  let flush_gpu memState = 
    Hashtbl.iter 
      (fun _ gpuVec -> Alloc.delete_gpu_vec memState.gpu_mem gpuVec) 
      memState.gpu_data; 
    Hashtbl.clear memState.gpu_data; 
    Hashtbl.clear memState.gpu_rev_lookup

  let alloc_gpu memState nbytes = 
    try Alloc.smart_alloc memState.gpu_mem nbytes 
    with _ -> (
      Printf.printf "[Alloc] Can't allocate %d bytes, flushing GPU\n";
      flush_gpu memState;  
      Alloc.smart_alloc memState.gpu_mem nbytes
    )
         
  let alloc_host memState nbytes = Alloc.smart_alloc memState.host_mem nbytes 

  let calc_nbytes len ty shape =
    let eltT = DynType.elt_type ty in
    let eltSize = DynType.sizeof eltT in
    len * eltSize 

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
    }
    in 
    let id = fresh_data_id ?refcount memState in 
    associate_id_with_gpu_data memState gpuVec id;  
    IFDEF DEBUG THEN
      Printf.printf "[Alloc] Created %s\n%!"  (GpuVal.gpu_vec_to_str gpuVec);
    ENDIF;  
    gpuVec   

  let mk_gpu_val memState t shape = 
    IFDEF DEBUG THEN assert (DynType.is_vec t); ENDIF; 
    let gpuVec = mk_gpu_vec memState t shape in 
    GpuVal.GpuArray gpuVec   

  let mk_host_vec memState ?refcount ?nbytes ty shape =
    let len = Shape.nelts shape in  
    let nbytes = match nbytes with 
      | None ->
         let eltT = DynType.elt_type ty in 
         let eltSize = DynType.sizeof eltT in
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
      Printf.printf "[Alloc] Created %s\n%!"  (HostVal.host_vec_to_str hostVec); 
    ENDIF;  
    hostVec
    
  let vec_from_gpu memState gpuVec = 
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
      Printf.printf "[Alloc] vector returned from GPU: %s\n"
        (HostVal.host_vec_to_str hostVec); 
    ENDIF;
    hostVec 
      
  let vec_to_gpu memState hostVec = 
    let gpuPtr = alloc_gpu memState hostVec.nbytes in  
    Cuda.cuda_memcpy_to_device hostVec.ptr gpuPtr hostVec.nbytes;
    let shapeDevPtr, shapeBytes = shape_to_gpu memState hostVec.shape in
    let gpuVec =  {
      vec_ptr = gpuPtr; 
      vec_nbytes = hostVec.nbytes;
      vec_len= Shape.nelts hostVec.shape; 
      
      vec_shape_ptr = shapeDevPtr;
      vec_shape_nbytes = shapeBytes; 
      
      vec_shape = hostVec.shape;
      vec_t = hostVec.host_t; 
      vec_slice_start=None; 
    }
    in 
    IFDEF DEBUG THEN 
      Printf.printf "[Alloc] vector sent to GPU: %s\n" 
        (GpuVal.gpu_vec_to_str gpuVec); 
    ENDIF;
    gpuVec    
end 
include ManagedAlloc   

module Scope = struct 
  let get_curr_env memState = 
    IFDEF DEBUG THEN 
      assert (not $ Stack.is_empty memState.envs);
    ENDIF;  
    Stack.top memState.envs  

  let push_env memState : unit = 
    if Stack.is_empty memState.envs then 
      Stack.push ID.Map.empty memState.envs 
    else 
      (* copy the current environment *) 
      Stack.push (get_curr_env memState) memState.envs
      
  let pop_env memState : unit =
    IFDEF DEBUG THEN 
      assert (not $ Stack.is_empty memState.envs); 
    ENDIF;   
    ignore (Stack.pop memState.envs) 
   
  let lookup memState id =
    let env = get_curr_env memState in 
     if ID.Map.mem id env then ID.Map.find id env
     else 
       failwith $ 
         Printf.sprintf "[MemoryState] variable %s not found!" (ID.to_str id) 


  let dec_old_binding_value memState env id = 
    if ID.Map.mem id env then (   
      dec_ref memState (ID.Map.find id env)
    )
    
  let set_binding memState id rhs =
    (* be sure to increment ref counts first, to avoid deleting something 
       when it was also the old value of a variable 
    *) 
    inc_ref memState rhs; 
    IFDEF DEBUG THEN 
      assert (not $ Stack.is_empty memState.envs); 
    ENDIF; 
    let env = Stack.pop memState.envs in
    dec_old_binding_value memState env id; 
    let env' = ID.Map.add id rhs env in 
    Stack.push env' memState.envs 
  
  let set_bindings memState ids vals = 
    IFDEF DEBUG THEN 
      assert (List.length ids = List.length vals);
      assert (not $ Stack.is_empty memState.envs);  
    ENDIF;
    List.iter (inc_ref memState) vals;
    let env = Stack.pop memState.envs in
    List.iter (dec_old_binding_value memState env) ids;   
    let env' = ID.Map.extend env ids vals in
    Stack.push env' memState.envs  

  let rec id_set_from_values memState = function 
    | [] -> DataId.Set.empty  
    | (InterpVal.Data id)::vs -> 
        DataId.Set.add id (id_set_from_values memState vs)
    | (InterpVal.Scalar _ )::vs -> id_set_from_values memState vs
    | (InterpVal.Array arr)::vs -> 
        ID.Set.union 
          (id_set_from_values memState (Array.to_list arr))
          (id_set_from_values memState vs)
    
  let enter_data_scope memState =
    Stack.push DataId.Set.empty memState.data_scopes


  let exit_data_scope ?(escaping_values=[]) memState = 
    let dataScope = Stack.pop memState.data_scopes in 
    let excludeSet = id_set_from_values memState escaping_values in
    (* remove the escaping IDs from the scope before freeing its contents *)
    let prunedScope = DataId.Set.diff dataScope excludeSet in
    (* 
    IFDEF DEBUG THEN 
      Printf.printf "[Scope] Leaving data scope w/ escaping values: %s\n"
        (String.concat ", " (List.map InterpVal.to_str escaping_values)); 
      Printf.printf "[Scope] Excluded from deletion by dec_ref: %s\n"
        (String.concat ", " 
          (List.map DataId.to_str (DataId.Set.elements excludeSet)));
      Printf.printf "[Scope] Decrementing references on: %s\n"
        (String.concat ", " 
          (List.map DataId.to_str (DataId.Set.elements prunedScope)));
    ENDIF;
    *) 
    DataId.Set.iter (dec_data_ref memState) prunedScope;
    (* we still decrement the counts on the exclude set, we 
       just expect them to be incremented again by an assignment 
    *) 
    DataId.Set.iter 
      (fun id -> set_refcount memState id (get_refcount memState id - 1))
      excludeSet
    ; 
    (* dump the excluded ids into the older data scope *)
    if Stack.is_empty memState.data_scopes then 
      Stack.push excludeSet memState.data_scopes 
    else   
      let olderScope = Stack.pop memState.data_scopes in
      Stack.push (DataId.Set.union excludeSet olderScope) memState.data_scopes 


  let enter_scope memState = 
    enter_data_scope memState;
    push_env memState

  let exit_scope ?(escaping_values=[]) memState = 
    exit_data_scope ~escaping_values memState; 
    pop_env memState
 
end
include Scope 

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
  | HostVal.HostScalar n -> InterpVal.Scalar n 
  | HostVal.HostArray  hostVec -> 
      InterpVal.Data (get_host_vec_id memState hostVec)
  | HostVal.HostBoxedArray boxedArray -> 
      let interpValArray = Array.map (add_host memState) boxedArray in 
      InterpVal.Array interpValArray 
  
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
  | GpuVal.GpuArray gpuVec -> InterpVal.Data (get_gpu_vec_id memState gpuVec)   
  | GpuVal.GpuScalar n -> InterpVal.Scalar n  


let is_on_gpu memState = function 
  | InterpVal.Data id -> Hashtbl.mem memState.gpu_data id 
  | InterpVal.Scalar _ -> true
  (* even if all the array's elements are on the GPU, no guarantee 
     they are a contiguous chunk 
  *)
  | InterpVal.Array _ -> false

let is_on_host memState = function 
  | InterpVal.Data id -> Hashtbl.mem memState.host_data id 
  | InterpVal.Scalar _ -> true
  | InterpVal.Array _ -> false
  

let get_scalar state = function 
 | InterpVal.Scalar n -> n
 | InterpVal.Data id -> assert false   
 | InterpVal.Array _ -> failwith "An array? How did that happen?"

let rec get_shape memState interpVal = match interpVal with   
  | InterpVal.Scalar _ -> Shape.scalar_shape 
  | InterpVal.Data id ->   
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
  | InterpVal.Array arr -> 
      (* assume uniform arrays *)
      let eltShape = get_shape memState arr.(0) in
      Shape.append_dim (Array.length arr) eltShape 

let rec get_type memState interpVal = 
  match interpVal with  
  | InterpVal.Scalar n -> PQNum.type_of_num n 
  | InterpVal.Data id -> 
      if is_on_gpu memState interpVal then
        let gpuVec = Hashtbl.find memState.gpu_data id in  
        gpuVec.vec_t   
      else (
        assert (is_on_host memState interpVal);
        let hostVec = Hashtbl.find memState.host_data id in
        hostVec.host_t  
      )
  | InterpVal.Array arr -> 
      (* for now, assume uniformity of element types *) 
      let eltT = get_type memState arr.(0) in 
      DynType.VecT eltT 


let rec sizeof memState interpVal = 
  let t = get_type memState interpVal in 
  match interpVal with  
  | InterpVal.Scalar n -> DynType.sizeof t  
  | InterpVal.Data _ -> 
      let s = get_shape memState interpVal in 
      Shape.nelts s * (DynType.sizeof (DynType.elt_type t))
  | InterpVal.Array arr ->  
      Array.fold_left (fun sum elt -> sum + sizeof memState elt) 0 arr 

let rec get_gpu memState = function 
  | InterpVal.Data id -> 
    if Hashtbl.mem memState.gpu_data id then
      GpuVal.GpuArray (Hashtbl.find memState.gpu_data id)
    else (
      let hostVec = Hashtbl.find memState.host_data id in
      IFDEF DEBUG THEN 
        Printf.printf "[MemoryState] Sending to GPU: --%s\n" 
          (HostVal.host_vec_to_str hostVec);
       ENDIF; 
      let gpuVec = vec_to_gpu memState hostVec in 
      associate_id_with_gpu_data memState gpuVec id;  
      GpuVal.GpuArray gpuVec
   )
  | InterpVal.Scalar n -> GpuVal.GpuScalar n
  (* WARNING: This is essentially a memory leak, since we leave 
     no data id associated with the gpu memory allocated here 
   *)   
  | InterpVal.Array arr ->
      (* for now, assume all rows are of uniform type/size *)
      let nrows = Array.length arr in   
      let elt = arr.(0) in 
      let eltSize = sizeof memState elt in
      let eltType = get_type memState elt in 
      let eltShape = get_shape memState elt in  
      let nbytes = nrows * eltSize in
      let finalType = DynType.VecT eltType in 
      let finalShape = Shape.append_dim nrows eltShape in 
      let destVec = mk_gpu_vec memState finalType finalShape in
      let destPtr = destVec.vec_ptr in 
      IFDEF DEBUG THEN 
        Printf.printf "[MemoryState] Transferring interpreter array to GPU\n";
        Printf.printf "[MemoryState] -- elts = %s \n" 
            (String.concat ", " (List.map InterpVal.to_str (Array.to_list arr)))
        ;  
        Printf.printf 
          "[MemoryState] -- elt size: %d, elt type: %s, elt shape: %s\n" 
            eltSize
            (DynType.to_str eltType)
            (Shape.to_str eltShape)
        ;
        Printf.printf 
          "[MemoryState] -- total size: %d, final type : %s,  final shape: %s\n"
          nbytes
          (DynType.to_str finalType)
          (Shape.to_str finalShape)
        ;  
      ENDIF; 
      for i = 0 to nrows - 1 do 
        let currPtr = Int64.add destPtr (Int64.of_int $ i * eltSize) in 
        match arr.(i) with 
          | InterpVal.Scalar (PQNum.Int32 i32) -> 
              Cuda.cuda_set_gpu_int32_vec_elt currPtr 0 i32 
          | InterpVal.Scalar (PQNum.Float32 f32) -> 
              Cuda.cuda_set_gpu_float32_vec_elt currPtr 0 f32 
          | InterpVal.Data id -> 
              if Hashtbl.mem memState.gpu_data id then 
                let eltVec = Hashtbl.find memState.gpu_data id in 
                IFDEF DEBUG THEN 
                  Printf.printf 
                    "[MemoryState] copying array elt to gpu address %Lx: %s\n"
                    currPtr
                    (GpuVal.gpu_vec_to_str eltVec)
                ENDIF; 
                Cuda.cuda_memcpy_device_to_device eltVec.vec_ptr currPtr eltSize 
              else
                let eltHostVec = Hashtbl.find memState.host_data id in
                IFDEF DEBUG THEN 
                  Printf.printf 
                    "[MemoryState] copying array elt to gpu address %Lx: %s\n"
                    currPtr
                    (HostVal.host_vec_to_str eltHostVec)
                ENDIF;  
                Cuda.cuda_memcpy_to_device eltHostVec.ptr currPtr eltSize  
          | _ -> assert false 
       done; 
       GpuVal.GpuArray destVec

let rec get_host memState interpVal = 
  match interpVal with  
  | InterpVal.Data id -> 
    if Hashtbl.mem memState.host_data id then
      HostVal.HostArray (Hashtbl.find memState.host_data id)
    else (
      IFDEF DEBUG THEN 
        Printf.printf 
          "[MemoryState->get_host] Initiating transfer from GPU to host\n%!"
        ;
      ENDIF; 
      let gpuVec = Hashtbl.find memState.gpu_data id in
      let hostVec = vec_from_gpu memState gpuVec in
      associate_id_with_host_data memState hostVec id; 
      IFDEF DEBUG THEN
        Printf.printf "[MemoryState->get_host] Got %s \n"
          (HostVal.host_vec_to_str hostVec);  
      ENDIF;
      HostVal.HostArray hostVec
    )
  | InterpVal.Scalar n -> HostVal.HostScalar n 
  | InterpVal.Array arr ->
      HostVal.HostBoxedArray (Array.map (get_host memState) arr)

module Slicing = struct 

  (* returns a gpu_val, not a gpu_vec since the elements might be scalars, 
     also increment the ref count of the thing we're slicing into 
   *)
  let slice_gpu_vec memState gpuVec idx : gpu_val = 
    let sliceShape = Shape.slice_shape gpuVec.vec_shape [0] in
    let sliceType = DynType.peel_vec gpuVec.vec_t in
    let sliceVal = 
      if DynType.is_scalar sliceType then index gpuVec idx 
      else (
        (* if the slice result is not a scalar, we have to increment 
           the reference count of the original data so we don't 
           leave this slice dangling by deleting its parent 
         *) 
        inc_data_ref memState (get_gpu_vec_id memState gpuVec); 
        let bytesPerElt = DynType.sizeof (DynType.elt_type sliceType) in
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
        }
      )
    in  
    IFDEF DEBUG THEN 
    Printf.printf 
      "[MemoryState] Got GPU slice index %d of %s\n" 
      idx
      (gpu_vec_to_str gpuVec)
    ; 
    Printf.printf "[MemoryState] GPU slice result: %s\n" (to_str sliceVal); 
    ENDIF; 
    sliceVal   

let slice_gpu_val memState gpuVal idx : gpu_val = 
  match gpuVal with   
  | GpuScalar _ -> failwith "can't slice a GPU scalar"
  | GpuArray gpuVec -> slice_gpu_vec memState gpuVec idx


  (* slice on the GPU or CPU? *) 
  let slice memState arr idx = match arr with 
    | InterpVal.Scalar _ -> failwith "[MemoryState] Can't slice a scalar"
    | InterpVal.Array arr -> arr.(idx)
    | InterpVal.Data id -> 
      if Hashtbl.mem memState.gpu_data id then 
        let gpuVec = Hashtbl.find memState.gpu_data id in
        add_gpu memState (slice_gpu_vec memState gpuVec idx)
      else 
        assert false
        (*
        let hostVec = Hashtbl.find memState.host_data id in 
        let sliceVec = host_slice hostVec idx in 
        add_host memState sliceVec
        *)   
end
include Slicing 