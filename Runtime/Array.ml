open Type 

(* question: how do we know which memory space the pointer is in? *) 
type t = { 
    ptr : Int64.t; 
    array_type : Type.t;
    elt_type : Type.elt_t;
    nelts : int; 
    nbytes : int; 
    elt_nbytes : int;  
    rank : int; 
    shape : Shape.t;
    shape_ptr : Int64.t;   
    strides : int array;
    strides_ptr: Int64.t;   
}