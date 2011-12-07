(* pp: -parser o pa_macro.cmo *)

open Base
open Imp 

type dim_op = Mult | Add | Max 
type dim = 
  | Const of int 
  | Dim of ID.t * int 
  | Op of dim_op * dim * dim 

type shape = dim list 
    
type env = shape ID.Map.t  


let rec dim_to_str = function 
  | Const c -> string_of_int c 
  | Dim (x, d) -> (ID.to_str x) ^ "[" ^ (string_of_int d) ^ "]"
  | Op(Mult, d1, d2) -> (dim_to_str d1) ^ " * " ^ (dim_to_str d2) 
  | Op(Add, d1, d2) -> (dim_to_str d1) ^ " + " ^ (dim_to_str d2) 
  | Op(Max, d1, d2) -> "max(" ^ (dim_to_str d1) ^ ", " ^ (dim_to_str d2) ^ ")" 

let dim_list_to_str dims = String.concat ", " (List.map dim_to_str dims) 

let to_str shape = "[" ^ (dim_list_to_str dims) ^ "]"
                
let scalar = [] 

let is_scalar s = (s=[])

let rank shape = List.length shape 

let get_dim shape d = 
  if rank shape < d-1 then failwith "Insufficient rank"
  else List.nth shape d 
  
let outer_dim shape = get_dim shape 0 
 

let peel_outer_dim = function 
  | [] -> []
  | _::tail -> tail  

let peel ?(axes=[0]) shape =
  if axes = [0] then peel_outer_dim shape 
  else 
    let rec aux i = function 
      | [] -> []
      | d::ds -> 
          if List.mem i axes then aux (i+1) ds 
          else d :: (aux (i+1) ds)
    in 
    aux i shape  
  
let peel_shape_list ?(axes=[0]) shapes = List.map (peel ~axes) shapes
    
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
  IFDEF DEBUG THEN 
    if maxRank <= 0 then failwith $ 
      Printf.sprintf 
        "expected at least one shape with rank higher than 0, received: %s"
        (String.concat ", " (List.map to_str shapes))
  ENDIF; 
  let peeledShapes =
    List.map2 
      (fun s r -> if r = maxRank then peel_shape s else s) 
      shapes
      ranks
  in
  let maxShapes = List.filter (fun s -> rank s = maxRank) shapes in
  let maxDim = Imp.max_exp_node_list (List.map outer_dim maxShapes) in 
  maxDim, peeledShapes
                 
let shape_to_str shape = "[" ^ (exp_node_list_to_str shape) ^ "]"
let shapes_to_str shapes = String.concat ", " (List.map shape_to_str shapes) 

(* get a list of all the dimensions of an Imp array *) 
let all_dims ( id : ID.t) (rank : int) : shape  =
  List.map (fun i -> Dim(id,i)) (List.range 0 (rank - 1))

let rec of_int_list = function 
  | [] -> [] 
  | i::rest -> (Const i) :: of_int_list rest


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
let rewrite_shapes env shapes = List.map (rewrite_shape env) shapes 

let concat s1 s2 = s1 @ s2 
let nelts = Imp.prod_exp_node_list 


let get_call_output_shapes fn (inputs : shape list) =  
  let replaceMap = 
    ID.Map.extend ID.Map.empty (Array.to_list fn.input_ids) inputs 
  in
  let rawOutputShapes = 
    List.map (Hashtbl.find fn.sizes) (Array.to_list fn.output_ids)
  in
  rewrite_shapes replaceMap rawOutputShapes
                