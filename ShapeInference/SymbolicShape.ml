(* pp: -parser o pa_macro.cmo *)

open Base

type dim_op = Mult | Add | Max 
type dim = 
  | Const of int 
  | Dim of ID.t * int 
  | Op of dim_op * dim * dim 

type t = dim list 
    
type env = t ID.Map.t  

let rec dim_to_str = function 
  | Const c -> string_of_int c 
  | Dim (x, d) -> (ID.to_str x) ^ "[" ^ (string_of_int d) ^ "]"
  | Op(Mult, d1, d2) -> (dim_to_str d1) ^ " * " ^ (dim_to_str d2) 
  | Op(Add, d1, d2) -> (dim_to_str d1) ^ " + " ^ (dim_to_str d2) 
  | Op(Max, d1, d2) -> "max(" ^ (dim_to_str d1) ^ ", " ^ (dim_to_str d2) ^ ")" 

let dim_list_to_str dims = String.concat ", " (List.map dim_to_str dims) 

let to_str shape = "[" ^ (dim_list_to_str shape) ^ "]"

let shapes_to_str shapes = String.concat ", " (List.map to_str shapes) 
                
let scalar = [] 

let is_scalar s = (s=[])

let const i = 
  assert (i >= 0); 
  Const i 

let zero = Const 0
let one = Const 1 
  
let dim x i = 
  assert (i >= 0); 
  Dim(x, i)

let add d1 d2 = match (d1,d2) with 
  | Const 0, _ -> d2
  | _, Const 0 -> d2 
  | Const x, Const y -> Const (x+y)
  | _  -> Op(Add, d1, d2)

let mult d1 d2 = match (d1, d2) with 
  | Const 0, _ 
  | _, Const 0 -> Const 0
  | _, Const 1 -> d1 
  | Const 1, d2 -> d2 
  | Const x, Const y -> Const (x*y)
  | _ -> Op(Mult, d1, d2)


let max_ d1 d2 = match (d1,d2) with 
    | Const 0, _ -> d2 
    | _, Const 0 -> d1 
    | Const x, Const y -> 
      if x > y then Const x 
      else Const y 
    | _ -> if d1 = d2 then d1 else Op(Max, d1, d2)  

let rec simplify_dim = function
  | Op(op, d1, d2) ->
    let d1' = simplify_dim d1 in 
    let d2' = simplify_dim d2 in 
    begin match op with 
      | Add -> add d1' d2' 
      | Mult -> mult d1' d2' 
      | Max -> max_ d1' d2' 
    end 
  | other -> other 

let max_of_dims shape = List.fold_left max_ zero shape 
let prod_of_dims shape = List.fold_left mult one shape 

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
    aux 0 shape  
  
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
  let ranks : int list = List.map rank shapes in 
  let maxRank = List.fold_left max 0 ranks in
  IFDEF DEBUG THEN 
    if maxRank <= 0 then failwith $ 
      Printf.sprintf 
        "expected at least one shape with rank higher than 0, received: %s"
        (String.concat ", " (List.map to_str shapes))
  ENDIF; 
  let peeledShapes =
    List.map2 
      (fun s r -> if r = maxRank then peel s else s) 
      shapes
      ranks
  in
  let maxShapes = List.filter (fun s -> rank s = maxRank) shapes in
  let maxDim = max_of_dims (List.map outer_dim maxShapes) in 
  maxDim, peeledShapes
                 

let all_dims ( id : ID.t) (rank : int) : t  =
  List.map (fun i -> Dim(id,i)) (List.range 0 (rank - 1))

let rec of_int_list = function 
  | [] -> [] 
  | i::rest -> (Const i) :: of_int_list rest


let rec rewrite_dim_helper (env:env) dim = match dim with 
  | Dim (id, d) -> 
      if ID.Map.mem id env then 
        let shape = ID.Map.find id env in
        get_dim shape d 
      else dim
  | Op(op, d1, d2) -> 
      let d1' = rewrite_dim_helper env d1 in 
      let d2' = rewrite_dim_helper env d2 in 
      Op(op, d1', d2')
  | _  -> dim 

let rewrite_dim env d = simplify_dim (rewrite_dim_helper env d)  

let rewrite_shape env shape = List.map (rewrite_dim env) shape   
let rewrite_shapes env shapes = List.map (rewrite_shape env) shapes 

let concat s1 s2 = s1 @ s2 
 

