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
   
and array_annot = 
  | SharedArray of int list 
  | PrivateArray of exp list 
  | OutputArray of exp list 
  | InputArray of int  
and fn = {
  input_ids : ID.t array;
  input_types : DynType.t array;
          
  output_ids : ID.t array; 
  output_types : DynType.t array;
  output_sizes : (ID.t, exp list) PMap.t; 
           
  (* all IDs which aren't inputs or outputs are locals *)     
  local_ids : ID.t array;   
  local_types : DynType.t array;   
  local_arrays : (ID.t, array_annot) PMap.t;
                
  body : block;
  tenv :(ID.t, DynType.t) PMap.t;
  (* doesn't count slices into the same array *) 
  shared_array_allocations : (ID.t, int list) PMap.t
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
        (args_to_str indices) 
        (exp_node_to_str rhs)
  | SyncThreads -> spaces ^ "syncthreads"
  | Comment s -> spaces ^ "// " ^ s
  (* used to plug one function into another, shouldn't exist in final code *) 
  | SPLICE -> spaces ^ "SPLICE"
and block_to_str ?(spaces="") stmts = 
  String.concat "\n" (List.map (stmt_to_str ~spaces) stmts)
and args_to_str exps = String.concat ", " (List.map exp_node_to_str exps) 
let fn_to_str fn =
  let inputs = List.map ID.to_str (Array.to_list fn.input_ids) in 
  let outputs = List.map ID.to_str (Array.to_list fn.output_ids) in 
  let bodyStr = block_to_str  fn.body in 
  sprintf "fn (%s) -> (%s) = {\n%s\n}" 
    (String.concat ", " inputs) 
    (String.concat ", " outputs) 
    bodyStr  
           


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

let select cond tNode fNode = 
  assert (tNode.exp_type = fNode.exp_type); 
  { exp = Select(tNode.exp_type, cond, tNode, fNode); 
    exp_type = tNode.exp_type
  } 
    
let idx arr idx = 
  let idx' = cast DynType.Int32T idx in  
  let eltT = DynType.elt_type arr.exp_type in  
  {exp= Idx(arr, idx'); exp_type=eltT }

let dim n x = int_exp $ (DimSize(n, x))
 
     

(* get a list of all the dimensions of an Imp array *) 
let all_dims ( x : exp_node) : exp_node list =
  let ndims = DynType.nest_depth x.exp_type in  
  List.map (fun i -> dim i x) (List.til ndims)

(* return list of dimsizes for value of largest type in the givern array *)
let largest_val ( exps : exp_node array ) : exp_node = 
  let maxExp = ref exps.(0) in   
  for i = 1 to Array.length exps - 1 do
    if DynType.is_structure_subtype !maxExp.exp_type exps.(i).exp_type then 
      maxExp := exps.(i)
  done; 
  !maxExp
    
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

let id_of = function 
  | {exp=Var id} -> id 
  | _ -> failwith "Imp: expected variable" 
