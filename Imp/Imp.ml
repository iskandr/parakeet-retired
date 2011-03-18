(* pp: -parser o pa_macro.cmo *)

open Base 

type coord = X | Y | Z
 
type exp = 
  | Var of ID.t
  | Idx of exp_node * exp_node  
  | Op of Prim.scalar_op * DynType.t * exp_node list 
  | Select of DynType.t * exp_node * exp_node * exp_node 
  | Const of PQNum.num 
  | Cast of DynType.t * exp_node  
  | DimSize of int * exp_node 
  | ThreadIdx of coord 
  | BlockIdx of coord 
  | BlockDim of coord 
  | GridDim of coord
and exp_node = { 
  exp : exp; 
  exp_type : DynType.t;  
} 
and stmt = 
  | If of exp_node * block * block
  | While of exp_node * block
  | Set of ID.t * exp_node 
  | SetIdx of ID.t * exp_node list * exp_node
  | SyncThreads
  | Comment of string
  (* used to plug one function into another, shouldn't exist in final code *) 
  | SPLICE 
and block = stmt list
   
and array_storage = 
  | Global
  | Private
  | Shared
  | Slice
and fn = {
  input_ids : ID.t array;
  input_id_set : ID.t MutableSet.t; 
  input_types : DynType.t array;
          
  output_ids : ID.t array; 
  output_id_set : ID.t MutableSet.t; 
  output_types : DynType.t array;
  
  local_id_set : ID.t MutableSet.t; 
  
  types : (ID.t, DynType.t) Hashtbl.t;
  
  (* only stores IDs/sizes belonging to arrays *)  
  sizes:  (ID.t, exp_node list) Hashtbl.t; 
  array_storage : (ID.t, array_storage) Hashtbl.t;
   
  body : block;
}


(* PRETTY PRINTING *) 
open Printf 

let coord_to_str = function 
  | X -> "x" | Y -> "y" | Z -> "z"

let rec exp_node_to_str e  = exp_to_str e.exp 
and exp_to_str = function 
  | Var id -> ID.to_str id  
  | Idx (e1, e2) -> sprintf "%s[%s]" (exp_node_to_str e1) (exp_node_to_str e2) 
  | Op (op, argT, args) -> 
    sprintf "%s:%s (%s)" 
      (Prim.scalar_op_to_str op)
      (DynType.to_str argT) 
      (exp_node_list_to_str args)
  | Select (t, cond, trueVal, falseVal) -> 
      sprintf "select:%s(%s, %s, %s)" 
        (DynType.to_str t)
        (exp_node_to_str cond)
        (exp_node_to_str trueVal)
        (exp_node_to_str falseVal)
  | Const n -> PQNum.num_to_str n 
  | Cast (tNew, e) -> 
      let tOld = e.exp_type in 
      sprintf "cast %s->%s (%s)" 
        (DynType.to_str tOld) 
        (DynType.to_str tNew) 
        (exp_node_to_str e)
  | DimSize (k, e) -> sprintf "dimsize(%s, %d)" (exp_node_to_str e) k
  | ThreadIdx c -> sprintf "threadidx.%s" (coord_to_str c)
  | BlockIdx c -> sprintf "blockidx.%s" (coord_to_str c)
  | BlockDim c -> sprintf "blockdim.%s" (coord_to_str c)
  | GridDim c -> sprintf "griddim.%s" (coord_to_str c)
  

and stmt_to_str ?(spaces="") = function 
  | If (cond, tBlock, fBlock) ->
      let tStr = 
        if tBlock <> [] then 
           "\n" ^ (block_to_str ~spaces:(spaces ^ "  ") tBlock)
        else "" 
      in 
      let fStr =
        if fBlock <> [] then 
           "\n" ^ (block_to_str ~spaces:(spaces ^ "  ") fBlock)
        else ""
      in       
      sprintf "%s if (%s) then { %s } \n else { %s }"
        spaces 
        (exp_node_to_str cond)
        tStr 
        fStr
  | While (cond, body) ->
      let bodyStr = 
        if body <> [] then "\n" ^ (block_to_str ~spaces:(spaces ^ "  ") body)
        else ""
      in  
      sprintf "%s while(%s) { %s }" spaces (exp_node_to_str cond) bodyStr 
        
  | Set (id, rhs) -> 
      sprintf "%s %s = %s" spaces (ID.to_str id) (exp_node_to_str rhs)  
  | SetIdx (id, indices, rhs) -> 
      sprintf "%s %s[%s] = %s"
        spaces 
        (ID.to_str id) 
        (exp_node_list_to_str indices) 
        (exp_node_to_str rhs)
  | SyncThreads -> spaces ^ "syncthreads"
  | Comment s -> spaces ^ "// " ^ s
  (* used to plug one function into another, shouldn't exist in final code *) 
  | SPLICE -> spaces ^ "SPLICE"
and block_to_str ?(spaces="") stmts = 
  String.concat "\n" (List.map (stmt_to_str ~spaces) stmts)
and exp_node_list_to_str exps = 
  String.concat ", " (List.map exp_node_to_str exps)
and array_storage_to_str = function 
  | Global -> "global"
  | Private -> "private"
  | Shared -> "shared"
  | Slice -> "slice"
and shared_to_str fn = 
  let s = ref "" in
  let extend_string id = 
    if Hashtbl.mem fn.array_storage id  && 
       not $ MutableSet.mem fn.input_id_set id then
        let currStr = 
          Printf.sprintf "  %s %s :: [%s]"
            (array_storage_to_str $ Hashtbl.find fn.array_storage id)   
            (ID.to_str id)
            (exp_node_list_to_str $ Hashtbl.find fn.sizes id)
         in s := !s ^"\n" ^ currStr 
  in
  if not $ MutableSet.is_empty fn.local_id_set then s := !s ^ "\n"; 
  MutableSet.iter  extend_string fn.local_id_set;
  if not $ MutableSet.is_empty fn.local_id_set then s := !s ^ "\n";  
  !s 
let fn_to_str fn =
  let id_to_str id  = 
    let t = Hashtbl.find fn.types id in 
    ID.to_str id ^ " : " ^ (DynType.to_str t)
  in 
  let inputs = List.map id_to_str (Array.to_list fn.input_ids)  in 
  let outputs = List.map id_to_str (Array.to_list fn.output_ids) in 
  let bodyStr = block_to_str  fn.body in 
  sprintf "fn (%s) -> (%s) = {%s\n%s\n}"
    (String.concat ", " inputs) 
    (String.concat ", " outputs)
    (shared_to_str fn)  
    bodyStr  
           
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
let typed_exp (t:DynType.t)  (e : exp) : exp_node = {exp=e; exp_type=t}
let bool_exp : exp->exp_node = typed_exp DynType.BoolT
let int16_exp : exp->exp_node = typed_exp DynType.Int16T   
let uint16_exp : exp->exp_node = typed_exp DynType.UInt16T
let int_exp : exp -> exp_node = typed_exp DynType.Int32T  
let uint_exp : exp -> exp_node = typed_exp DynType.UInt32T  
let f32_exp : exp -> exp_node = typed_exp DynType.Float32T 
let f64_exp : exp -> exp_node = typed_exp DynType.Float64T 

(* CAST AN EXPRESSION TO A NEW TYPE 
   (or leave it alone if it's already that type
*)

let cast t expNode =
  let tOld = expNode.exp_type in  
  if tOld = t then expNode 
  else match expNode.exp with 
    | Const n -> { exp = Const (PQNum.coerce_num n t); exp_type=t}
    | _ -> 
      if DynType.is_scalar_subtype tOld t 
         || DynType.sizeof tOld = DynType.sizeof t 
      then 
         typed_exp t $ Cast(t,expNode) 
      else failwith $ 
        Printf.sprintf "[imp->cast] cannot create cast from %s to %s : %s"
          (DynType.to_str tOld)
          (DynType.to_str t)
          (exp_node_to_str expNode)
 


let common_type ?t args =
 match t with 
 | Some t -> t 
 | None -> 
     let types = List.map (fun node -> node.exp_type) args in
     let properCommonType = DynType.fold_type_list types in
     (* special case: cast mix of signed and unsigned 32-bit integers 
        to signed
     *)
     if List.for_all (fun t -> DynType.sizeof t = 4) types 
       && DynType.sizeof properCommonType > 4  then DynType.Int32T 
     else properCommonType 

(* arguments are either of the type explicitly passed as an argument or 
   you get the greatest common type between them. 
   Cast each argument to this greatest type, and then create a node 
   for the given operation. 
*)
let typed_op op ?t args =
  let argType = common_type ?t args in 
  assert (DynType.is_scalar argType);    
  typed_exp argType $ Op (op,argType,List.map (cast argType) args)


(* Same as typed_op, but with comparison operators which always return bools *) 
let cmp_op op ?t args =
  let argType = common_type ?t args in 
  assert (DynType.is_scalar argType);    
  typed_exp DynType.BoolT $ Op (op, argType, List.map (cast argType) args)

(* CUDA stuff *)
type vec3 = { x: exp_node; y: exp_node; z: exp_node}
let mk_vec3 (f : coord -> exp_node) : vec3  = { x = f X; y = f Y; z = f Z} 

let threadIdx = mk_vec3 (fun coord -> uint16_exp $ ThreadIdx coord)
let blockIdx = mk_vec3 (fun coord -> uint16_exp $ BlockIdx coord) 
let blockDim = mk_vec3 (fun coord -> uint16_exp $ BlockDim coord)
let gridDim = mk_vec3 (fun coord -> uint16_exp $ GridDim coord)

 

(* GENERAL IMP EXPRESSIONS *)
let uint32 i = uint_exp $ Const (PQNum.Int32 i)    
let int32 i = int_exp $ Const (PQNum.Int32 i)

let uint i = uint_exp $ Const (PQNum.Int32 (Int32.of_int i))
let int i =  int_exp $ Const (PQNum.Int32 (Int32.of_int i))
  
let float f = f32_exp $ Const (PQNum.Float32 f)  
let double d = f64_exp $ Const (PQNum.Float64 d) 

let bool b = bool_exp $ Const (PQNum.Bool b) 

let zero = int 0 
let one = int 1
 
let infinity = 
  typed_exp DynType.Float64T (Const (PQNum.Inf DynType.Float64T))
  
let neg_infinity = 
  typed_exp DynType.Float64T (Const (PQNum.NegInf DynType.Float64T))

let select cond tNode fNode = 
  assert (tNode.exp_type = fNode.exp_type); 
  { exp = Select(tNode.exp_type, cond, tNode, fNode); 
    exp_type = tNode.exp_type
  } 
    
let idx arr idx = 
  let idx' = cast DynType.Int32T idx in  
  let eltT = DynType.peel_vec arr.exp_type in  
  {exp= Idx(arr, idx'); exp_type=eltT }

let dim n x = int_exp $ (DimSize(n, x))
 
     
let len x = uint_exp $ DimSize(1, x)

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


let not_ x = typed_op Prim.Not ~t:DynType.BoolT [x] 
let (!$) = not_ 
 
let and_ x y = typed_op Prim.And ~t:DynType.BoolT [x;y]
let (&&$) = and_ 
 
let or_ x y = typed_op Prim.Or ~t:DynType.BoolT [x;y]
let (||$) = or_  

let sqrt32 x = typed_op Prim.Sqrt ~t:DynType.Float32T [x] 
let sqrt64 x = typed_op Prim.Sqrt ~t:DynType.Float64T [x]  

let ln_32 x = typed_op Prim.Log ~t:DynType.Float32T [x]
let ln_64 x = typed_op Prim.Log ~t:DynType.Float64T [x] 

let id_of = function 
  | {exp=Var id} -> id 
  | _ -> failwith "Imp: expected variable" 

let var ?(t=DynType.BottomT) id = { exp = Var id; exp_type = t}


let max_simplify d1 d2 = if d1.exp = d2.exp then d1 else max_ d1 d2

let mul_simplify d1 d2 = match d1.exp, d2.exp with 
  | Const n1, _ when PQNum.is_inf n1 -> infinity
  | _, Const n2 when PQNum.is_inf n2 -> infinity 
  | Const n1, _ when PQNum.is_zero n1 -> zero
  | _, Const n2 when PQNum.is_zero n2 -> zero
  | Const n1, _ when PQNum.is_one n1 -> d2
  | _, Const n2 when PQNum.is_one n2 -> d1
  | Const (PQNum.Int16 x), Const (PQNum.Int16 y) -> 
    {d1  with exp =  Const (PQNum.Int16 (x * y)) }
  | Const (PQNum.Int32 x), (Const PQNum.Int32 y) -> 
    {d1  with exp =  Const (PQNum.Int32 (Int32.mul x y)) }
  | Const (PQNum.Float32 x), (Const PQNum.Float32 y) -> 
    {d1  with exp =  Const (PQNum.Float32 (x *. y)) }
  | Const (PQNum.Float64 x), (Const PQNum.Float64 y) -> 
    {d1  with exp =  Const (PQNum.Float64 (x *. y)) }
  | _ -> mul d1 d2 

let add_simplify d1 d2 = match d1.exp, d2.exp with 
  | Const n1, _ when PQNum.is_inf n1 -> infinity
  | _, Const n2 when PQNum.is_inf n2 -> infinity 
  | Const n1, _ when PQNum.is_zero n1 -> d2 
  | _, Const n2 when PQNum.is_zero n2 -> d1
  | Const (PQNum.Int16 x), Const (PQNum.Int16 y) -> 
    {d1  with exp = Const (PQNum.Int16 (x + y)) }
  | Const (PQNum.Int32 x), (Const PQNum.Int32 y) -> 
    {d1  with exp =  Const (PQNum.Int32 (Int32.add x y)) }
  | Const (PQNum.Float32 x), (Const PQNum.Float32 y) -> 
    {d1  with exp = Const (PQNum.Float32 (x +. y)) }
  | Const (PQNum.Float64 x), (Const PQNum.Float64 y) -> 
    {d1 with exp = Const (PQNum.Float64 (x +. y)) } 
  | _ -> add d1 d2 

let rec fold_exp_node_list f default = function 
  | [] -> default
  | [e] -> e 
  | e::es -> f e (fold_exp_node_list f default es)

let max_exp_node_list es = fold_exp_node_list max_simplify neg_infinity es
let sum_exp_node_list es = fold_exp_node_list add_simplify zero es 
let prod_exp_node_list es = fold_exp_node_list mul_simplify one es 
