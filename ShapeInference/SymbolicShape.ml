open Base
open Imp 

type dim = exp_node 
type shape = dim list
type env = shape ID.Map.t  

let scalar = [] 

let is_scalar s = (s=[])

let rank shape = List.length shape 

let get_dim shape d = 
  if rank shape < d-1 then failwith "Insufficient rank"
  else List.nth shape d 
  
let outer_dim shape = get_dim shape 0 
 
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
  List.map (fun i -> dim i x) (List.range 0 (ndims-1))

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
        get_dim shape d 
      else expNode 
  | Imp.Op(op, t, args) ->
      let args' = List.map (rewrite_dim_helper env) args in 
      {expNode with Imp.exp = Imp.Op(op,t,args') }
  | _  -> expNode    

let rewrite_dim env d = 
  let d' = rewrite_dim_helper env d in 
  ImpSimplify.simplify_arith d' 

let rewrite_shape env shape = List.map (rewrite_dim env) shape   

let concat s1 s2 = s1 @ s2 
let nelts = Imp.prod_exp_node_list 
let to_str shape = Imp.exp_node_list_to_str shape
        