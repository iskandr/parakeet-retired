open Base
open Imp 
     


(* IMP STATEMENTS *)
let syncthreads = SyncThreads
let if_ cond t f  = If(cond,t,f)
let ifTrue cond t = if_ cond t []  
let while_ cond code = While(cond, code)
let comment str = Comment str


 (*
let rec collect_rev_indices_from_node node = collect_rev_indices node.exp 
and collect_rev_indices = function  
  | Var id -> id, [] 
  | Idx (lhs, idx) -> 
     let id, otherIndices = collect_rev_indices_from_node lhs in 
     id, idx :: otherIndices
  | _ -> failwith "[set] Expected variable"
   
let collect_indices exp = 
  let id, indices = collect_rev_indices exp in  
  id, List.rev indices 
  
let collect_indices_from_node node = collect_indices node.exp  
*)  
     
let set v rhs = match v.exp with 
  | Val {value=Var id} -> Set(id,rhs)
  | Idx ({value=Var id}, indices) -> SetIdx(id, indices,rhs)
  | _ -> assert false 
  
let rec setidx v indices rhs = match v.exp with 
  | Val {value=Var id} -> SetIdx(id, indices, rhs)
  | Idx ({value=Var id}, indices') -> SetIdx(id, indices' @ indices, rhs) 
  | other -> assert false 
  
 
(* HELPER FUNCTIONS FOR IMP EXPRESSIONS *)
let wrap_bool_val (v : value) : value_node = {value=v; value_type = ImpType.bool_t}
let wrap_char_val (v : value) : value_node = { value=v; value_type = ImpType.char_t}
let wrap_int16_val (v : value) : value_node = { value=v; value_type = ImpType.int16_t}
let wrap_int32_val (v : value) : value_node = { value=v; value_type = ImpType.int32_t}
let wrap_int64_val (v : value) : value_node = { value=v; value_type = ImpType.int32_t}
let wrap_float32_val (v : value) : value_node = { value=v; value_type = ImpType.float32_t}
let wrap_float64_val (v : value) : value_node = { value=v; value_type = ImpType.float64_t}

let exp_of_val (v : value_node) : exp_node = { exp_type = v.value_type; exp = Val v} 

let wrap_exp (e:exp) (ty:ImpType.t) = { exp = e; exp_type = ty } 
let wrap_bool_exp (e:exp) = {exp = e; exp_type = ImpType.bool_t}
let wrap_char_exp (e:exp) = { exp = e; exp_type = ImpType.char_t}
let wrap_int16_exp (e:exp) = { exp = e; exp_type = ImpType.int16_t}
let wrap_int32_exp (e:exp) = { exp = e; exp_type = ImpType.int32_t }
let wrap_int64_exp (e:exp) = { exp = e; exp_type = ImpType.int64_t } 
let wrap_float32_exp (e:exp) = { exp = e; exp_type = ImpType.float32_t} 
let wrap_float64_exp (e:exp) = { exp = e; exp_type = ImpType.float64_t } 


(*
let typed_exp (t:ImpType.t)  (e : exp) : exp_node = {exp=e; exp_type=ImpType.ScalarT t}

let bool_exp : exp->exp_node = typed_exp ImpType.bool_t
let int16_exp : exp->exp_node = typed_exp ImpType.int16_t 
let int_exp : exp -> exp_node = typed_exp ImpType.int32_t    
let f32_exp : exp -> exp_node = typed_exp ImpType.float32_t
let f64_exp : exp -> exp_node = typed_exp ImpType.float64_t 
*)
(* CAST AN EXPRESSION TO A NEW TYPE 
   (or leave it alone if it's already that type
*)

let cast (t:ImpType.t) (valNode:value_node) : exp_node =
  let old_t = valNode.value_type in
  if old_t = t then {exp = Val valNode; exp_type = t}
  else (   
    assert (ImpType.is_scalar old_t);  
    let elt_t : Type.elt_t = ImpType.elt_type t in
    let old_elt_t : Type.elt_t = ImpType.elt_type old_t in 
    match valNode.value with 
    | Const n -> 
      let n' = ParNum.coerce n elt_t in 
      { exp = Val { value = Const n'; value_type = t}; exp_type = t }
    | _ ->
      if Type.is_scalar_subtype old_elt_t elt_t || Type.sizeof old_elt_t = Type.sizeof elt_t 
      then {exp_type = t; exp = Cast(t, valNode)} 
      else failwith $ 
        Printf.sprintf "[imp->cast] cannot create cast from %s to %s : %s"
          (ImpType.to_str old_t)
          (ImpType.to_str t)
          (val_node_to_str valNode)
 )
let common_type ?t (args : value_node list) =
 match t with 
 | Some t -> ImpType.ScalarT t 
 | None -> 
     let types = List.map (fun node -> node.value_type) args in
     ImpType.combine_type_list types

let typed_op op ?t (args : value_node list) : exp_node =
  let argType = common_type ?t args in 
  assert (ImpType.is_scalar argType);    
  let eltT = ImpType.elt_type argType in 
  wrap_exp (Op (eltT, op, args)) argType 

(* Same as typed_op, but with comparison operators which always return bools *) 
let cmp_op op ?t args =
  let argType = common_type ?t args in 
  assert (ImpType.is_scalar argType);
  let eltT = ImpType.elt_type argType in
  wrap_bool_exp $ Op (eltT, op, args)

(* CUDA stuff *)
type vec3 = { x: value_node; y: value_node; z: value_node}
let mk_vec3 (f : coord -> value_node) : vec3  = { x = f X; y = f Y; z = f Z} 

let threadIdx = mk_vec3 (fun coord -> wrap_int16_val $ CudaInfo(ThreadIdx, coord))
let blockIdx = mk_vec3 (fun coord -> wrap_int16_val $ CudaInfo(BlockIdx, coord)) 
let blockDim = mk_vec3 (fun coord -> wrap_int16_val $ CudaInfo(BlockDim, coord))
let gridDim = mk_vec3 (fun coord -> wrap_int16_val $ CudaInfo (GridDim, coord))


(* GENERAL IMP EXPRESSIONS *)
let int32 i = wrap_int32_val $ Const (ParNum.Int32 i)
let int32_exp i = exp_of_val (int32 i)

let int i =  wrap_int32_val $ Const (ParNum.Int32 (Int32.of_int i))
let int_exp i = exp_of_val (int i)  
  
let float f = wrap_float32_val $ Const (ParNum.Float32 f)
let float_exp f = exp_of_val (float f)
    
let double d = wrap_float64_val $ Const (ParNum.Float64 d) 
let double_exp d = exp_of_val (double d) 

let bool b = wrap_bool_val $ Const (ParNum.Bool b) 
let bool_exp b = exp_of_val (bool b)

let zero = int 0 
let zero_exp = exp_of_val zero 

let one = int 1
let one_exp = exp_of_val one 

(* 
let infinity = 
  typed_exp Type.Float64T (Const (ParNum.Inf Type.Float64T))
  
let neg_infinity = 
  typed_exp Type.Float64T (Const (ParNum.NegInf Type.Float64T))
*)

let select cond t f = 
  assert (t.value_type = f.value_type); 
  { exp = Select(t.value_type, cond, f, f); 
    exp_type = t.value_type
  } 

let idx arr indices =
  let arrT = arr.value_type in 
  assert (ImpType.is_array arrT); 
  let numIndices = List.length indices in 
  assert (numIndices = ImpType.rank arrT); 
  let eltT = ImpType.elt_type arrT in   
  { exp = Idx(arr, indices); exp_type = ImpType.ScalarT eltT }

let dim (arr:value_node) (idx:value_node) = wrap_int32_exp $ (DimSize(idx, arr))
 
let len x = dim (int 0) x 

let max_ ?t x y = typed_op Prim.Max ?t [x;y]
let min_ ?t x y = typed_op Prim.Min ?t [x;y]  

let mul ?t x y = typed_op Prim.Mult ?t [x;y] 
let ( *$ ) = mul 

let add ?t x y = typed_op Prim.Add ?t [x; y]
let ( +$ ) = add

let div ?t  x y = typed_op Prim.Div ?t [x; y]
let ( /$ ) = div 

let sub ?t x y = typed_op Prim.Sub ?t [x; y]
let ( -$ ) = sub 

let mod_ ?t  x y = typed_op Prim.Mod ?t  [x; y]
let ( %$ ) = mod_ 

let safe_div_ ?t x y = typed_op Prim.SafeDiv ?t [x;y]

let lt ?t x y = cmp_op Prim.Lt ?t  [x; y]
let ( <$ ) = lt
 
let lte ?t  x y = cmp_op Prim.Lte ?t [x; y]
let ( <=$ ) = lte 

let gt ?t  x y = cmp_op Prim.Gt ?t  [x;y]
let ( >$ ) = gt 

let gte ?t  x y = cmp_op Prim.Gte ?t  [x;y]
let ( >=$ ) = gte 

let eq ?t x y = cmp_op Prim.Eq ?t  [x;y]
let ( =$ ) = eq 

let neq ?t x y = cmp_op Prim.Neq ?t  [x;y]
let ( <>$ ) = neq 


let not_ x = typed_op Prim.Not ~t:Type.BoolT [x] 
let (!$) = not_ 
 
let and_ x y = typed_op Prim.And ~t:Type.BoolT [x;y]
let (&&$) = and_ 
 
let or_ x y = typed_op Prim.Or ~t:Type.BoolT [x;y]
let (||$) = or_  

let sqrt32 x = typed_op Prim.Sqrt ~t:Type.Float32T [x] 
let sqrt64 x = typed_op Prim.Sqrt ~t:Type.Float64T [x]  

let ln_32 x = typed_op Prim.Log ~t:Type.Float32T [x]
let ln_64 x = typed_op Prim.Log ~t:Type.Float64T [x] 

let id_of_val valNode = match valNode.value with 
  | Var id -> id
  | _ -> assert false 
 
let id_of_exp (expNode:exp_node) = match expNode.exp with 
  | Val v -> id_of_val v  
  | _ -> failwith "Imp: expected variable" 

let var ~ty id =  {value = Var id; value_type = ty}
let var_exp ~ty id = exp_of_val (var ~ty id) 

let max_simplify (d1:value_node) (d2:value_node) : exp_node = 
  if d1.value = d2.value then exp_of_val d1  else max_ d1 d2

let mul_simplify (d1:value_node) (d2:value_node) = 
  match d1.value, d2.value with 
  | Const n1, _ when ParNum.is_zero n1 -> zero_exp
  | _, Const n2 when ParNum.is_zero n2 -> zero_exp
  | Const n1, _ when ParNum.is_one n1 -> exp_of_val d2
  | _, Const n2 when ParNum.is_one n2 -> exp_of_val d1
  | Const (ParNum.Int16 x), Const (ParNum.Int16 y) -> 
    exp_of_val {d1  with value =  Const (ParNum.Int16 (x * y)) }
  | Const (ParNum.Int32 x), (Const ParNum.Int32 y) -> 
    exp_of_val {d1  with value =  Const (ParNum.Int32 (Int32.mul x y)) }
  | Const (ParNum.Float32 x), (Const ParNum.Float32 y) -> 
    exp_of_val {d1  with value =  Const (ParNum.Float32 (x *. y)) }
  | Const (ParNum.Float64 x), (Const ParNum.Float64 y) -> 
    exp_of_val {d1  with value =  Const (ParNum.Float64 (x *. y)) }
  | _ -> mul d1 d2 

let add_simplify (d1:value_node) (d2:value_node) = 
  match d1.value, d2.value with 
  | Const n1, _ when ParNum.is_zero n1 -> exp_of_val d2 
  | _, Const n2 when ParNum.is_zero n2 -> exp_of_val d1
  | Const (ParNum.Int16 x), Const (ParNum.Int16 y) -> 
    exp_of_val {d1  with value = Const (ParNum.Int16 (x + y)) }
  | Const (ParNum.Int32 x), (Const ParNum.Int32 y) -> 
    exp_of_val {d1  with value =  Const (ParNum.Int32 (Int32.add x y)) }
  | Const (ParNum.Float32 x), (Const ParNum.Float32 y) -> 
    exp_of_val {d1  with value = Const (ParNum.Float32 (x +. y)) }
  | Const (ParNum.Float64 x), (Const ParNum.Float64 y) -> 
    exp_of_val {d1 with value = Const (ParNum.Float64 (x +. y)) } 
  | _ -> add d1 d2 
(*
let rec fold_val_node_list f (default : exp_node) = function 
  | [] -> default
  | [v] -> exp_of_val v 
  | v::vs -> f v (fold_exp_node_list f default es)
*)

(*
let max_exp_node_list es = fold_exp_node_list max_simplify neg_infinity es
let sum_exp_node_list es = fold_exp_node_list add_simplify zero es 
let prod_exp_node_list es = fold_exp_node_list mul_simplify one es 
*)
(*
let highest_rank_exp ( exps : exp_node array ) : exp_node = 
  let maxExp = ref exps.(0) in   
  for i = 1 to Array.length exps - 1 do
    if Type.is_structure_subtype !maxExp.exp_type exps.(i).exp_type then 
      maxExp := exps.(i)
  done; 
  !maxExp
*)  
