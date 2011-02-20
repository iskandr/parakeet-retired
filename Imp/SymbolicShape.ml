open Base
open Imp 

type shape = exp_node list

let peel_shape = function 
  | [] -> [] 
  | _::tailDims -> tailDims 

let peel_shape_list shapes = List.map peel_shape shapes
    
let split_shape = function 
  | [] -> assert false 
  | dim::dims -> dim, dims 

let rec split_shape_list = function 
  | [] -> [], [] 
  | s::rest -> 
      let d, shape = split_shape s in 
      let moreDims, moreShapes = split_shape_list rest in 
      d::moreDims, shape::moreShapes   

let rank shape = List.length shape 

let rec mk_max_dim = function 
  | [] -> assert false 
  | [dim] -> dim 
  | d::dims ->
      let d' = mk_max_dim dims in 
      if d.exp = d'.exp then d' else max_ d d'
         
let shape_to_str shape = "[" ^ (exp_node_list_to_str shape) ^ "]"
let shapes_to_str shapes = String.concat ", " (List.map shape_to_str shapes) 

(* get a list of all the dimensions of an Imp array *) 
let all_dims ( x : exp_node) : exp_node list =
  let ndims = DynType.nest_depth x.exp_type in  
  List.map (fun i -> dim i x) (List.til ndims)

(* return list of dimsizes for value of largest type in the given array *)
let largest_val ( exps : exp_node array ) : exp_node = 
  let maxExp = ref exps.(0) in   
  for i = 1 to Array.length exps - 1 do
    if DynType.is_structure_subtype !maxExp.exp_type exps.(i).exp_type then 
      maxExp := exps.(i)
  done; 
  !maxExp

let rec of_int_list = function 
  | [] -> [] 
  | i::rest -> (Imp.int i) :: of_int_list rest 

        