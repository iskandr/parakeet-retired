open Imp 
     
let always_const expNode = match expNode.exp with
  | Const _  
  | DimSize _   
  | ThreadIdx _   
  | BlockIdx _  
  | BlockDim _  
  | GridDim _ -> true 
  | _ -> false  

(* IMP STATEMENTS *)
let syncthreads = SyncThreads
let if_ cond t f  = If(cond,t,f)
let ifTrue cond t = if_ cond t []  
let while_ cond code = While(cond, code)
let comment str = Comment str


 
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
  
     
let set v rhs = match v.exp with 
  | Var id -> Set(id,rhs)
  | other -> 
    let id, indices = collect_indices other in 
    SetIdx(id, indices,rhs)
  
let rec setidx v indices rhs = match v.exp with 
  | Var id -> SetIdx(id, indices, rhs)
  | other -> 
     let id, indices' = collect_indices other in 
     SetIdx(id, indices' @ indices, rhs)
  
 
(* HELPER FUNCTIONS FOR IMP EXPRESSIONS *)
let typed_exp (t:Type.elt_t)  (e : exp) : exp_node = {exp=e; exp_type=ImpType.ScalarT t}
let bool_exp : exp->exp_node = typed_exp Type.BoolT
let int16_exp : exp->exp_node = typed_exp Type.Int16T 
let int_exp : exp -> exp_node = typed_exp Type.Int32T    
let f32_exp : exp -> exp_node = typed_exp Type.Float32T 
let f64_exp : exp -> exp_node = typed_exp Type.Float64T 

(* CAST AN EXPRESSION TO A NEW TYPE 
   (or leave it alone if it's already that type
*)

let cast (t:Type.elt_t) expNode =
  let tOld = ImpType.get_elt_type expNode.exp_type in  
  if tOld = t then expNode 
  else match expNode.exp with 
    | Const n -> { exp = Const (ParNum.coerce_num n t); exp_type=ImpType.ScalarT t}
    | _ -> 
      if Type.is_scalar_subtype tOld t 
         || Type.sizeof tOld = Type.sizeof t 
      then 
         typed_exp t $ Cast(t,expNode) 
      else failwith $ 
        Printf.sprintf "[imp->cast] cannot create cast from %s to %s : %s"
          (Type.elt_to_str tOld)
          (Type.elt_to_str t)
          (exp_node_to_str expNode)
 


let common_type ?t args =
 match t with 
 | Some t -> t 
 | None -> 
     let types = List.map (fun node -> node.exp_type) args in
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
  typed_exp argType $ Op (op,argType,List.map (cast argType) args)


(* Same as typed_op, but with comparison operators which always return bools *) 
let cmp_op op ?t args =
  let argType = common_type ?t args in 
  assert (Type.is_scalar argType);    
  typed_exp Type.BoolT $ Op (op, argType, List.map (cast argType) args)

(* CUDA stuff *)
type vec3 = { x: exp_node; y: exp_node; z: exp_node}
let mk_vec3 (f : coord -> exp_node) : vec3  = { x = f X; y = f Y; z = f Z} 

let threadIdx = mk_vec3 (fun coord -> uint16_exp $ ThreadIdx coord)
let blockIdx = mk_vec3 (fun coord -> uint16_exp $ BlockIdx coord) 
let blockDim = mk_vec3 (fun coord -> uint16_exp $ BlockDim coord)
let gridDim = mk_vec3 (fun coord -> uint16_exp $ GridDim coord)


(* GENERAL IMP EXPRESSIONS *)
let uint32 i = uint_exp $ Const (ParNum.Int32 i)    
let int32 i = int_exp $ Const (ParNum.Int32 i)

let uint i = uint_exp $ Const (ParNum.Int32 (Int32.of_int i))
let int i =  int_exp $ Const (ParNum.Int32 (Int32.of_int i))
  
let float f = f32_exp $ Const (ParNum.Float32 f)  
let double d = f64_exp $ Const (ParNum.Float64 d) 

let bool b = bool_exp $ Const (ParNum.Bool b) 

let zero = int 0 
let one = int 1
 
let infinity = 
  typed_exp Type.Float64T (Const (ParNum.Inf Type.Float64T))
  
let neg_infinity = 
  typed_exp Type.Float64T (Const (ParNum.NegInf Type.Float64T))

let select cond tNode fNode = 
  assert (tNode.exp_type = fNode.exp_type); 
  { exp = Select(tNode.exp_type, cond, tNode, fNode); 
    exp_type = tNode.exp_type
  } 
    
let idx arr idx = 
  let idx' = cast Type.Int32T idx in  
  let eltT = Type.peel_vec arr.exp_type in  
  {exp= Idx(arr, idx'); exp_type=eltT }

let dim (n:int) (x:exp_node) = int_exp $ (DimSize(int n, x))
 
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
  