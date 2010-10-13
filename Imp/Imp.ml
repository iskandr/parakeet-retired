open Base 

type coord = X | Y | Z
 
type exp = 
  | Var of ID.t
  | Idx of exp_node * exp_node  
  | Op of Prim.scalar_op * DynType.t * exp_node list 
  | Select of DynType.t * exp_node * exp_node * exp_node 
  | Const of PQNum.num 
  | Cast of DynType.t * DynType.t *  exp_node  
  | DimSize of int * exp_node 
  | ThreadIdx of coord 
  | BlockIdx of coord 
  | BlockDim of coord 
  | GridDim of coord
and exp_node = { 
  imp_exp : exp; 
  imp_type : DynType.t;  
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


(*let rec infer_dyn_type tenv = function 
  | Var id ->  PMap.find id tenv   
  | Const n -> PQNum.type_of_num n 
  | ThreadIdx _ -> DynType.UInt16T
  | BlockIdx _ -> DynType.UInt16T 
  | BlockDim _ -> DynType.UInt16T  
  | GridDim _ ->  DynType.UInt16T 
  | DimSize _ -> DynType.UInt32T   
  | Idx (arr, _) -> DynType.peel_vec (infer_dyn_type tenv arr)  
  | Op (_,t,_, _)
  | Select (t,_,_,_) 
  | Cast (t,_,_) -> t
 *)


(* IMP STATEMENTS *)
let syncthreads = SyncThreads
let if_ cond t f  = If(cond,t,f)
let ifTrue cond t = if_ cond t []  
let while_ cond code = While(cond, code)
let comment str = Comment str

let rec collect_nested_indices e = match e.imp_exp with  
  | Var id -> id, [] 
  | Idx (lhs, idx) -> 
     let id, otherIndices = collect_nested_indices lhs in 
     id, idx :: otherIndices
  | _ -> failwith "[set] Expected variable"
 
let set v rhs = match v.imp_exp with 
  | Var id -> Set(id,rhs)
  | other -> 
    let id, indices = collect_nested_indices v in SetIdx(id, indices,rhs)
  
  
let rec setidx v indices rhs = match v.imp_exp with 
  | Var id -> SetIdx(id, indices, rhs)
  | other -> 
     let id, indices' = collect_nested_indices v in 
     SetIdx(id, indices' @ indices, rhs)
  
 
(* HELPER FUNCTIONS FOR IMP EXPRESSIONS *)
let typed_exp (t:DynType.t)  (e : exp) : exp_node = {imp_exp=e; imp_type=t}
let bool_exp : exp->exp_node = typed_exp DynType.BoolT
let int16_exp : exp->exp_node = typed_exp DynType.Int16T   
let int_exp : exp -> exp_node = typed_exp DynType.Int32T  
let f32_exp : exp -> exp_node = typed_exp DynType.Float32T 
let f64_exp : exp -> exp_node = typed_exp DynType.Float64T 


(* CUDA stuff *)
type vec3 = { x: exp_node; y: exp_node; z: exp_node}
let mk_vec3 (f : coord -> exp_node) : vec3  = { x = f X; y = f Y; z = f Z} 

let threadIdx = mk_vec3 (fun coord -> int16_exp $ ThreadIdx coord)
let blockIdx = mk_vec3 (fun coord -> int16_exp $ BlockIdx coord) 
let blockDim = mk_vec3 (fun coord -> int16_exp $ BlockDim coord)
let gridDim = mk_vec3 (fun coord -> int16_exp $ GridDim coord)

 

(* GENERAL IMP EXPRESSIONS *) 
let idx arr idx = 
  assert (idx.imp_type = DynType.Int32T); 
  let eltT =DynType.elt_type arr.imp_type in  
  {imp_exp= Idx(arr, idx); imp_type=eltT}

let dim n x = int_exp $ DimSize(n,x) 
let len x = int_exp $ DimSize(1, x)

let int32 i = int_exp $ Const (PQNum.Int32 i) 
let int i = int_exp $ Const (PQNum.Int32 (Int32.of_int i))  
  
let float f = f32_exp $ Const (PQNum.Float32 f)  
  
let double d = f64_exp $ Const (PQNum.Float64 d)  

let mul t x y = typed_exp t $ Op (Prim.Mult,t,[x;y]) 
let add t x y = typed_exp t $ Op(Prim.Add,t,[x;y])
let div t x y = typed_exp t $  Op(Prim.Div,t,[x;y])
let sub t x y = typed_exp t $ Op(Prim.Sub,t,[x;y])
let mod_ t x y = typed_exp t $ Op(Prim.Mod,t,[x;y])

let lt t x y = bool_exp $ Op(Prim.Lt,t,[x;y]) 
let lte t x y = bool_exp $ Op(Prim.Lte,t,[x;y])
let gt t x y = bool_exp $ Op(Prim.Gt,t,[x;y])
let gte t x y = bool_exp $ Op(Prim.Gte,t,[x;y])
let eq t x y = bool_exp $ Op(Prim.Eq,t,[x;y])
let neq t x y = bool_exp $ Op(Prim.Neq,t,[x;y])

let not_ x = bool_exp $ Op (Prim.Not,DynType.BoolT,[x])
let and_ x y = bool_exp $ Op(Prim.And,DynType.BoolT,[x;y])
let or_ x y = bool_exp $ Op(Prim.Or,DynType.BoolT,[x;y])

let sqrt32 x =
  assert (x.imp_type = DynType.Float32T); 
  f32_exp $ Op(Prim.Sqrt, DynType.Float32T, [x])
  
let sqrt64 x = 
  assert (x.imp_type = DynType.Float64T);
  f64_exp $ Op(Prim.Sqrt, DynType.Float64T,[x])

let id_of = function 
  | {imp_exp=Var id} -> id 
  | _ -> failwith "Imp: expected variable" 

open Printf 

let coord_to_str = function 
  | X -> "x" | Y -> "y" | Z -> "z"


let rec exp_node_to_str e  = exp_to_str e.imp_exp 
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
  | Cast (tNew, tOld, e) -> 
       sprintf "cast %s->%s (%s)" 
        (DynType.to_str tOld) (DynType.to_str tNew) (exp_node_to_str e)
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
           
   
