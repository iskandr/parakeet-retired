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
and fn = { input_ids : ID.t array;
           input_types : DynType.t array;
           output_ids : ID.t array; 
           output_types : DynType.t array;
           (* all IDs which aren't inputs or outputs are locals *)     
           local_ids : ID.t array;   
           local_types : DynType.t array;         
           body : block;
           tenv :(ID.t,DynType.t) PMap.t; 
           shared : (ID.t, int list) PMap.t
         }




(* IMP STATEMENTS *)
let syncthreads = SyncThreads
let if_ cond t f  = If(cond,t,f)
let ifTrue cond t = if_ cond t []  
let while_ cond code = While(cond, code)
let comment str = Comment str

let rec collect_nested_indices e = match e.exp with  
  | Var id -> id, [] 
  | Idx (lhs, idx) -> 
     let id, otherIndices = collect_nested_indices lhs in 
     id, idx :: otherIndices
  | _ -> failwith "[set] Expected variable"
 
let set v rhs = match v.exp with 
  | Var id -> Set(id,rhs)
  | other -> 
    let id, indices = collect_nested_indices v in SetIdx(id, indices,rhs)
  
  
let rec setidx v indices rhs = match v.exp with 
  | Var id -> SetIdx(id, indices, rhs)
  | other -> 
     let id, indices' = collect_nested_indices v in 
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
    | Const n -> {expNode with exp = Const (PQNum.coerce_num n t); exp_type=t}
    | _ -> 
      if DynType.is_scalar_subtype tOld t 
         || DynType.sizeof tOld = DynType.sizeof t 
      then 
         typed_exp t $ Cast(t,expNode) 
      else failwith $ 
        Printf.sprintf "[imp->cast] cannot create cast from %s to %s"
          (DynType.to_str tOld)
          (DynType.to_str t)
 


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
let int32 i = 
  if i < Int32.zero then int_exp $ Const (PQNum.Int32 i)
  else uint32 i 

let uint i = uint_exp $ Const (PQNum.Int32 (Int32.of_int i))
let int i = 
  if i < 0 then 
    int_exp $ Const (PQNum.Int32 (Int32.of_int i))
  else uint i    
  
let float f = f32_exp $ Const (PQNum.Float32 f)  
let double d = f64_exp $ Const (PQNum.Float64 d) 


    
let idx arr idx = 
  let idx' = cast DynType.UInt32T idx in  
  let eltT = DynType.elt_type arr.exp_type in  
  {exp= Idx(arr, idx'); exp_type=eltT }

let dim n x = uint_exp $ DimSize(n, x) 
let len x = uint_exp $ DimSize(1, x)
 

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

let id_of = function 
  | {exp=Var id} -> id 
  | _ -> failwith "Imp: expected variable" 

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
      (args_to_str args)
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

and stmt_to_str = function 
  | If (cond, tBlock, fBlock) -> 
      sprintf "if (%s) then { %s } else { %s }" 
        (exp_node_to_str cond)
        (block_to_str tBlock)
        (block_to_str fBlock) 
  | While (cond, body) -> 
      sprintf "while(%s) { %s }" (exp_node_to_str cond) (block_to_str body)
  | Set (id, rhs) -> 
      sprintf "%s = %s" (ID.to_str id) (exp_node_to_str rhs)  
  | SetIdx (id, indices, rhs) -> 
      sprintf "%s[%s] = %s" 
        (ID.to_str id) 
        (args_to_str indices) 
        (exp_node_to_str rhs)
  | SyncThreads -> "syncthreads"
  | Comment s -> "// " ^ s
  (* used to plug one function into another, shouldn't exist in final code *) 
  | SPLICE -> "SPLICE"
and block_to_str stmts = String.concat "\n" (List.map stmt_to_str stmts)
and args_to_str exps = String.concat ", " (List.map exp_node_to_str exps) 
let fn_to_str fn =
  let inputs = List.map ID.to_str (Array.to_list fn.input_ids) in 
  let outputs = List.map ID.to_str (Array.to_list fn.output_ids) in 
  let bodyStr = block_to_str  fn.body in 
  sprintf "fn (%s) -> (%s) = {\n%s\n}" 
    (String.concat ", " inputs) 
    (String.concat ", " outputs) 
    bodyStr  
           
