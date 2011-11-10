type t = { 
    memspace_id : MemId.t; 
    data_ptr : Ptr.t; 
    shape_ptr : Ptr.t;
    strides_ptr: Ptr.t;
    
    array_type : Type.t;
    elt_type : Type.elt_t;
    
    rank : int;
    nelts : int; 
    nbytes : int; 
    shape : Shape.t;
    strides : int array;
}


let array_type {array_type} = array_type  
let elt_type {elt_type} = elt_type 
let shape {shape} = shape  