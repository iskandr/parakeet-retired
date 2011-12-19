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

let cast (elt_t:Type.elt_t) (valNode:value_node) : exp_node =
  let old_t = valNode.value_type in 
  assert (ImpType.is_scalar old_t);  
  let old_elt_t : Type.elt_t = ImpType.get_elt_type old_t in
  if old_elt_t = elt_t then {exp = Val valNode; exp_type = valNode.value_type}
  else 
    let ty = ImpType.ScalarT elt_t in 
    match valNode.value with 
    | Const n -> 
      let n' = ParNum.coerce n elt_t in 
      { exp = Val { value = Const n'; value_type = ty}; exp_type = ty }
    | _ ->
      if Type.is_scalar_subtype old_elt_t elt_t || Type.sizeof old_elt_t = Type.sizeof elt_t 
      then {exp_type = ty; exp = Cast(ty,valNode)} 
      else failwith $ 
        Printf.sprintf "[imp->cast] cannot create cast from %s to %s : %s"
          (ImpType.to_str old_t)
          (ImpType.to_str ty)
          (val_node_to_str valNode)
 
let common_type ?t (args : value_node list) =
 match t with 
 | Some t -> t 
 | None -> 
     let types = List.map (fun node -> node.value_type) args in
     let properCommonType = Type.fold_type_list types in
     (* special case: cast mix of signed and unsigned 32-bit integers 
        to signed
     *)
     if List.for_all (fun t -> Type.sizeof t = 4) types 
       && Type.sizeof properCommonType > 4  then Type.Int32T 
     else properCommonType 

(* arguments are either of the type explicitly passed as an argument or 
   you get the greatest common type between them. 
   Cast each argument to this greatest type, and then create a node 
   for the given operation. 
*)
let typed_op op ?t args =
  let argType = common_type ?t args in 
  assert (Type.is_scalar argType);    
  let eltT = Type.elt_type argType in 
  let args' =  List.map (cast eltT) args in 
  wrap_exp (Op (op, eltT, args')) argType 

(* Same as typed_op, but with comparison operators which always return bools *) 
let cmp_op op ?t args =
  let argType = common_type ?t args in 
  assert (Type.is_scalar argType);
  let args' = List.map (cast argType) args' in      
  let eltT = Type.elt_type argType in
  bool_exp $ Op (op, eltT, args')

(* CUDA stuff *)
type vec3 = { x: value_node; y: value_node; z: value_node}
let mk_vec3 (f : coord -> value_node) : vec3  = { x = f X; y = f Y; z = f Z} 

let threadIdx = mk_vec3 (fun coord -> int16_val $ ThreadIdx coord)
let blockIdx = mk_vec3 (fun coord -> int16_val $ BlockIdx coord) 
let blockDim = mk_vec3 (fun coord -> int16_val $ BlockDim coord)
let gridDim = mk_vec3 (fun coord -> int16_val $ GridDim coord)


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
  { exp = Select(t.value_type, cond, tNode, fNode); 
    exp_type = t.value_type
  } 
    
let idx arr indices =
  let indices' = List.map (cast Type.Int32T) indices in
  let arrT = arr.value_type in 
  assert (Type.is_array arrT); 
  let eltT = Type.peel ~num_axes:(List.length indices') arrT in  
  { exp = Idx(arr, indices'); exp_type= eltT }

let dim (arr:value_node) (idx:value_node) = int_exp $ (DimSize(int n, x))
 
let len x = dim 0 x 

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

let id_of = function 
  | {exp=Var id} -> id 
  | _ -> failwith "Imp: expected variable" 

let var ?(t=Type.BottomT) id = { exp = Var id; exp_type = t}


let max_simplify d1 d2 = if d1.exp = d2.exp then d1 else max_ d1 d2

let mul_simplify d1 d2 = match d1.exp, d2.exp with 
  | Const n1, _ when ParNum.is_inf n1 -> infinity
  | _, Const n2 when ParNum.is_inf n2 -> infinity 
  | Const n1, _ when ParNum.is_zero n1 -> zero
  | _, Const n2 when ParNum.is_zero n2 -> zero
  | Const n1, _ when ParNum.is_one n1 -> d2
  | _, Const n2 when ParNum.is_one n2 -> d1
  | Const (ParNum.Int16 x), Const (ParNum.Int16 y) -> 
    {d1  with exp =  Const (ParNum.Int16 (x * y)) }
  | Const (ParNum.Int32 x), (Const ParNum.Int32 y) -> 
    {d1  with exp =  Const (ParNum.Int32 (Int32.mul x y)) }
  | Const (ParNum.Float32 x), (Const ParNum.Float32 y) -> 
    {d1  with exp =  Const (ParNum.Float32 (x *. y)) }
  | Const (ParNum.Float64 x), (Const ParNum.Float64 y) -> 
    {d1  with exp =  Const (ParNum.Float64 (x *. y)) }
  | _ -> mul d1 d2 

let add_simplify d1 d2 = match d1.exp, d2.exp with 
  | Const n1, _ when ParNum.is_inf n1 -> infinity
  | _, Const n2 when ParNum.is_inf n2 -> infinity 
  | Const n1, _ when ParNum.is_zero n1 -> d2 
  | _, Const n2 when ParNum.is_zero n2 -> d1
  | Const (ParNum.Int16 x), Const (ParNum.Int16 y) -> 
    {d1  with exp = Const (ParNum.Int16 (x + y)) }
  | Const (ParNum.Int32 x), (Const ParNum.Int32 y) -> 
    {d1  with exp =  Const (ParNum.Int32 (Int32.add x y)) }
  | Const (ParNum.Float32 x), (Const ParNum.Float32 y) -> 
    {d1  with exp = Const (ParNum.Float32 (x +. y)) }
  | Const (ParNum.Float64 x), (Const ParNum.Float64 y) -> 
    {d1 with exp = Const (ParNum.Float64 (x +. y)) } 
  | _ -> add d1 d2 


let rec fold_exp_node_list f default = function 
  | [] -> default
  | [e] -> e 
  | e::es -> f e (fold_exp_node_list f default es)



let max_exp_node_list es = fold_exp_node_list max_simplify neg_infinity es
let sum_exp_node_list es = fold_exp_node_list add_simplify zero es 
let prod_exp_node_list es = fold_exp_node_list mul_simplify one es 

(*
let highest_rank_exp ( exps : exp_node array ) : exp_node = 
  let maxExp = ref exps.(0) in   
  for i = 1 to Array.length exps - 1 do
    if Type.is_structure_subtype !maxExp.exp_type exps.(i).exp_type then 
      maxExp := exps.(i)
  done; 
  !maxExp
*)  
