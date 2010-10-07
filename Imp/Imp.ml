open Base 

type ty = DynType.t

type coord = X | Y | Z 
type exp = 
  | Var of ID.t
  | Idx of exp * exp  
  | Op of Prim.scalar_op * ty * ty * exp list 
  | Select of ty * exp * exp * exp 
  | Const of PQNum.num 
  | Cast of DynType.t * DynType.t *  exp  
  | DimSize of int * exp 
  | ThreadIdx of coord 
  | BlockIdx of coord 
  | BlockDim of coord 
  | GridDim of coord 
and stmt = 
  | If of exp * block * block
  | While of exp * block
  | Set of ID.t * exp 
  | SetIdx of ID.t * exp list * exp
  | SyncThreads
  | Comment of string
  (* used to plug one function into another, shouldn't exist in final code *) 
  | SPLICE 
and block = stmt list
and fn = { input_ids : ID.t array;
           input_types : ty array;
           output_ids : ID.t array; 
           output_types : ty array;
           (* all IDs which aren't inputs or outputs are locals *)     
           local_ids : ID.t array;   
           local_types : ty array;         
           body : block;
           tenv :(ID.t,DynType.t) PMap.t; 
           shared : (ID.t, int list) PMap.t
         }


let is_simple_exp = function 
  | Var _ 
  | Const _ -> true 
  | _ -> false  

let is_compound_exp e = not (is_simple_exp e)

let rec infer_dyn_type tenv = function 
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
 

(* CUDA stuff *)
type vec3 = { x: exp; y: exp; z: exp}
let threadIdx = { x=ThreadIdx X; y = ThreadIdx Y; z=ThreadIdx Z}
let blockIdx = {x =BlockIdx X; y=BlockIdx Y; z=BlockIdx Z}
let blockDim = {x=BlockDim X; y=BlockDim Y; z=BlockDim Z}
let gridDim = {x=GridDim X; y=GridDim Y; z=GridDim Z}
let syncthreads = SyncThreads 

(* IMP STATEMENTS *)
let if_ cond t f  = If(cond,t,f)
let ifTrue cond t = if_ cond t []  
let while_ cond code = While(cond, code)
let comment str = Comment str

let rec collect_nested_indices = function 
  | Var id -> id, [] 
  | Idx (lhs, idx) -> 
     let id, otherIndices = collect_nested_indices lhs in 
     id, idx :: otherIndices
  | _ -> failwith "[set] Expected variable"
 
let set v rhs = match v with 
  | Var id -> Set(id,rhs)
  | other -> 
    let id, indices = collect_nested_indices other in SetIdx(id, indices,rhs)
  
  
let rec setidx v indices rhs = match v with 
  | Var id -> SetIdx(id, indices, rhs)
  | other -> 
     let id, indices' = collect_nested_indices other in 
     SetIdx(id, indices' @ indices, rhs)
  
 
(* IMP EXPRESSIONS *)
let idx arr idx = Idx(arr, idx)

let dim n x = DimSize(n,x) 
let len x = DimSize(1, x)


let int32 i = Const (PQNum.Int32 i)
let int i = Const (PQNum.Int32 (Int32.of_int i))
let float f = Const (PQNum.Float32 f)
let double d = Const (PQNum.Float64 d)

let mul t x y = Op (Prim.Mult,t,t,[x;y])
let add t x y = Op(Prim.Add,t,t,[x;y])
let div t x y = Op(Prim.Div,t,t,[x;y])
let sub t x y = Op(Prim.Sub,t,t,[x;y])
let mod_ t x y = Op(Prim.Mod,t,t,[x;y])

let lt t x y = Op(Prim.Lt,DynType.BoolT,t,[x;y])
let lte t x y = Op(Prim.Lte,DynType.BoolT,t,[x;y])
let gt t x y = Op(Prim.Gt,DynType.BoolT,t,[x;y])
let gte t x y = Op(Prim.Gte,DynType.BoolT,t,[x;y])
let eq t x y = Op(Prim.Eq,DynType.BoolT,t,[x;y])
let neq t x y = Op(Prim.Neq,DynType.BoolT,t,[x;y])

let not_ x = Op (Prim.Not,DynType.BoolT,DynType.BoolT,[x])
let and_ x y = Op(Prim.And,DynType.BoolT,DynType.BoolT,[x;y])
let or_ x y = Op(Prim.Or,DynType.BoolT,DynType.BoolT,[x;y])

let sqrt32 x = Op(Prim.Sqrt,DynType.Float32T, DynType.Float32T,[x])
let sqrt64 x = Op(Prim.Sqrt, DynType.Float64T, DynType.Float64T,[x])

let id_of = function Var id -> id | _ -> failwith "Imp: expected variable" 

open Printf 

let coord_to_str = function 
  | X -> "x" | Y -> "y" | Z -> "z"

let rec exp_to_str = function 
  | Var id -> ID.to_str id  
  | Idx (e1, e2) -> sprintf "%s[%s]" (exp_to_str e1) (exp_to_str e2) 
  | Op (op, _, argT, args) -> 
    sprintf "%s:%s (%s)" 
      (Prim.scalar_op_to_str op)
      (DynType.to_str argT) 
      (args_to_str args)
  | Select (t, cond, trueVal, falseVal) -> 
      sprintf "select:%s(%s, %s, %s)" 
        (DynType.to_str t)
        (exp_to_str cond)
        (exp_to_str trueVal)
        (exp_to_str falseVal)
  | Const n -> PQNum.num_to_str n 
  | Cast (tNew, tOld, e) -> 
       sprintf "cast %s->%s (%s)" 
        (DynType.to_str tOld) (DynType.to_str tNew) (exp_to_str e)
  | DimSize (k, e) -> sprintf "dimsize(%s, %d)" (exp_to_str e) k  
  | ThreadIdx c -> sprintf "threadidx.%s" (coord_to_str c)
  | BlockIdx c -> sprintf "blockidx.%s" (coord_to_str c)
  | BlockDim c -> sprintf "blockdim.%s" (coord_to_str c)
  | GridDim c -> sprintf "griddim.%s" (coord_to_str c)

and stmt_to_str = function 
  | If (cond, tBlock, fBlock) -> 
      sprintf "if (%s) then { %s } else { %s }" 
        (exp_to_str cond)
        (block_to_str tBlock)
        (block_to_str fBlock) 
  | While (cond, body) -> 
      sprintf "while(%s) { %s }" (exp_to_str cond) (block_to_str body)
  | Set (id, rhs) -> 
      sprintf "%s = %s" (ID.to_str id) (exp_to_str rhs)  
  | SetIdx (id, indices, rhs) -> 
      sprintf "%s[%s] = %s" 
        (ID.to_str id) 
        (args_to_str indices) 
        (exp_to_str rhs)
  | SyncThreads -> "syncthreads"
  | Comment s -> "// " ^ s
  (* used to plug one function into another, shouldn't exist in final code *) 
  | SPLICE -> "SPLICE"
and block_to_str stmts = String.concat "\n" (List.map stmt_to_str stmts)
and args_to_str exps = String.concat ", " (List.map exp_to_str exps) 
let fn_to_str fn =
  let inputs = List.map ID.to_str (Array.to_list fn.input_ids) in 
  let outputs = List.map ID.to_str (Array.to_list fn.output_ids) in 
  let bodyStr = block_to_str  fn.body in 
  sprintf "fn (%s) -> (%s) = {\n%s\n}" 
    (String.concat ", " inputs) 
    (String.concat ", " outputs) 
    bodyStr  
           
   
