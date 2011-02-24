open Base
open Imp 

type dim = exp_node 
type shape = dim list
type env = shape ID.Map.t  

let scalar = [] 

let outer_dim shape = List.hd shape
 
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

let max_dim d1 d2 = if d1.exp = d2.exp then d1 else Imp.max_ d1 d2 

let rec max_dim_of_list = function 
  | [] -> assert false 
  | [dim] -> dim 
  | d::dims -> max_dim d (max_dim_of_list dims)   
      


(* peels the maximum dim off shapes of maximal rank, leaving others
   untouched. combine all the maxdims into a single expression *) 
let split_max_rank shapes = 
  let ranks = List.map rank shapes in 
  let maxRank = List.fold_left max 0 ranks in
  assert (maxRank > 0); 
  let peeledShapes =
    List.map2 
      (fun s r -> if r = maxRank then peel_shape s else s) 
      shapes
      ranks
  in
  let maxShapes = List.filter (fun s -> rank s = maxRank) shapes in
  let maxDim = max_dim_of_list (List.map outer_dim maxShapes) in 
  maxDim, peeledShapes
                 
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


let rec rewrite_dim_helper env expNode = match expNode.Imp.exp with 
  | Imp.DimSize (d, {Imp.exp = Imp.Var id}) -> 
      if ID.Map.mem id env then 
        let shape = ID.Map.find id env in
        (if List.length shape < d then failwith "Insufficient rank");  
        List.nth shape d 
      else expNode 
  | Imp.Op(op, t, args) ->
      let args' = List.map (rewrite_dim_helper env) args in 
      {expNode with Imp.exp = Imp.Op(op,t,args') }
  | _  -> expNode    

let rewrite_dim env d = 
  let d' = rewrite_dim_helper env d in 
  ImpSimplify.simplify_arith d' 

let rewrite_shape env shape = List.map (rewrite_dim env) shape   
        