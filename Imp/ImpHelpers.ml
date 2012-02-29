open Base
open Imp

(* IMP STATEMENTS *)
let syncthreads = SyncThreads
let if_ cond t f  = If(cond,t,f)
let ifTrue cond t = if_ cond t []
let while_ cond code = While(cond, code)
let comment str = Comment str

let set v rhs = match v.value  with
  | Var id -> Set(id,rhs)
  | _ -> assert false

let rec setidx arr indices rhs = SetIdx(arr, indices, rhs)

(* HELPER FUNCTIONS FOR IMP EXPRESSIONS *)
let wrap_bool (v : value) : value_node =
  {value=v; value_type = ImpType.bool_t}
let wrap_char (v : value) : value_node =
  { value=v; value_type = ImpType.char_t}
let wrap_int16 (v : value) : value_node =
  { value=v; value_type = ImpType.int16_t}
let wrap_int32 (v : value) : value_node =
  { value=v; value_type = ImpType.int32_t}
let wrap_int64 (v : value) : value_node =
  { value=v; value_type = ImpType.int32_t}
let wrap_float32 (v : value) : value_node =
  { value=v; value_type = ImpType.float32_t}
let wrap_float64 (v : value) : value_node =
  { value=v; value_type = ImpType.float64_t}


let wrap (v:value) (ty:ImpType.t) = { value = v; value_type = ty }

(* CAST AN EXPRESSION TO A NEW TYPE
   (or leave it alone if it's already that type
*)

let cast (t:ImpType.t) (valNode:value_node) : value_node =
  let old_t = valNode.value_type in
  if old_t = t then valNode
  else (
    assert (ImpType.is_scalar old_t);
    let elt_t : Type.elt_t = ImpType.elt_type t in
    let old_elt_t : Type.elt_t = ImpType.elt_type old_t in
    match valNode.value with
    | Const n ->
      let n' = ParNum.coerce n elt_t in
      { value = Const n'; value_type = t}
    | _ ->
      let sameSize = Type.sizeof old_elt_t = Type.sizeof elt_t in
      if  sameSize || Type.is_scalar_subtype old_elt_t elt_t then
        {value_type = t; value = Cast(t, valNode)}
      else failwith $
        Printf.sprintf "[imp->cast] cannot create cast from %s to %s : %s"
          (ImpType.to_str old_t)
          (ImpType.to_str t)
          (value_node_to_str valNode)
   )
let common_type ?t (args : value_node list) =
 match t with
 | Some t -> ImpType.ScalarT t
 | None ->
     let types = List.map (fun node -> node.value_type) args in
     ImpType.combine_type_list types

let typed_op op ?t (args : value_node list) : value_node =
  let argType = common_type ?t args in
  assert (ImpType.is_scalar argType);
  let eltT = ImpType.elt_type argType in
  wrap (Op (eltT, op, args)) argType

(* Same as typed_op, but with comparison operators which always return bools *)
let cmp_op op ?t args =
  let argType = common_type ?t args in
  assert (ImpType.is_scalar argType);
  let eltT = ImpType.elt_type argType in
  wrap_bool $ Op (eltT, op, args)

(* CUDA stuff *)
type vec3 = { x: value_node; y: value_node; z: value_node}
let mk_vec3 (f : coord -> value_node) : vec3  = { x = f X; y = f Y; z = f Z}

let threadIdx =
  mk_vec3 (fun coord -> wrap_int16 $ CudaInfo(ThreadIdx, coord))
let blockIdx =
  mk_vec3 (fun coord -> wrap_int16 $ CudaInfo(BlockIdx, coord))
let blockDim =
  mk_vec3 (fun coord -> wrap_int16 $ CudaInfo(BlockDim, coord))
let gridDim =
  mk_vec3 (fun coord -> wrap_int16 $ CudaInfo (GridDim, coord))


(* GENERAL IMP EXPRESSIONS *)
let int32 i = wrap_int32 $ Const (ParNum.Int32 i)
let int i =  wrap_int32 $ Const (ParNum.Int32 (Int32.of_int i))
let float f = wrap_float32 $ Const (ParNum.Float32 f)
let double d = wrap_float64 $ Const (ParNum.Float64 d)
let bool b = wrap_bool $ Const (ParNum.Bool b)

let zero = int 0
let one = int 1

let select cond t f =
  assert (t.value_type = f.value_type);
  { value = Select(t.value_type, cond, f, f);
    value_type = t.value_type
  }


let dim (arr:value_node) (idx:value_node) = wrap_int32 $ (DimSize( arr, idx))

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

let ln_32 x = typed_op Prim.Ln ~t:Type.Float32T [x]
let ln_64 x = typed_op Prim.Ln ~t:Type.Float64T [x]

let id_of_value valNode = match valNode.value with
  | Var id -> id
  | _ -> assert false

let var ~ty id =  {value = Var id; value_type = ty}

let max_simplify (d1:value_node) (d2:value_node) : value_node =
  if d1.value = d2.value then d1 else max_ d1 d2

let mul_simplify (d1:value_node) (d2:value_node) =
  match d1.value, d2.value with
  | Const n1, _ when ParNum.is_zero n1 -> zero
  | _, Const n2 when ParNum.is_zero n2 -> zero
  | Const n1, _ when ParNum.is_one n1 -> d2
  | _, Const n2 when ParNum.is_one n2 -> d1
  | Const (ParNum.Int16 x), Const (ParNum.Int16 y) ->
    {d1  with value =  Const (ParNum.Int16 (x * y)) }
  | Const (ParNum.Int32 x), (Const ParNum.Int32 y) ->
    {d1  with value =  Const (ParNum.Int32 (Int32.mul x y)) }
  | Const (ParNum.Float32 x), (Const ParNum.Float32 y) ->
    {d1  with value =  Const (ParNum.Float32 (x *. y)) }
  | Const (ParNum.Float64 x), (Const ParNum.Float64 y) ->
    {d1  with value =  Const (ParNum.Float64 (x *. y)) }
  | _ -> mul d1 d2

let add_simplify (d1:value_node) (d2:value_node) : value_node  =
  match d1.value, d2.value with
  | Const n1, _ when ParNum.is_zero n1 -> d2
  | _, Const n2 when ParNum.is_zero n2 -> d1
  | Const (ParNum.Int16 x), Const (ParNum.Int16 y) ->
    {d1  with value = Const (ParNum.Int16 (x + y)) }
  | Const (ParNum.Int32 x), (Const ParNum.Int32 y) ->
    {d1  with value =  Const (ParNum.Int32 (Int32.add x y)) }
  | Const (ParNum.Float32 x), (Const ParNum.Float32 y) ->
    {d1  with value = Const (ParNum.Float32 (x +. y)) }
  | Const (ParNum.Float64 x), (Const ParNum.Float64 y) ->
    {d1 with value = Const (ParNum.Float64 (x +. y)) }
  | _ -> add d1 d2


(* assume indices are into sequential dims *)
let idx arr indices =
  let arrT = arr.value_type in
  (* for convenience, treat indexing into scalars as the identity operation *)
  if ImpType.is_scalar arrT then arr
  else begin
    assert (ImpType.is_array arrT);
    let numIndices = List.length indices in
    let rank = ImpType.rank arrT in
    if numIndices <> rank then
      failwith $ Printf.sprintf
        "[ImpHelpers] Expected %d indices, got %d"
        rank
        numIndices
    ;
    let eltT = ImpType.elt_type arrT in
    { value = Idx(arr, indices); value_type = ImpType.ScalarT eltT }
  end


let is_const_int {value} = match value with
  | Const n -> ParNum.is_int n
  | _ -> false

let get_const_int {value} = match value with
  | Const n -> ParNum.to_int n
  | _ -> failwith "Not an integer!"

let fixdim ~arr ~dim ~idx : value_node  =
  {
    value = FixDim(arr, dim, idx);
    value_type = ImpType.peel ~num_axes:1 arr.value_type
  }

(* recursively build fixdim nodes for a list of indices *)
let rec fixdims ~arr ~dims ~indices : value_node =
   match dims, indices with
   | d::ds, i::is -> fixdims ~arr:(fixdim arr d i ) ~dims:ds ~indices:is
   | [], [] -> arr
   | _ -> failwith "Expected dims and indices to be of same length"


let slice ~arr ~dim ~start ~stop =
  { value = Slice(arr, dim, start, stop);
    value_type = arr.value_type
  }

let copy x : value_node =
  let t = x.value_type in
  if ImpType.is_scalar t then x
  else { value = Copy x; value_type = ImpType.type_of_copy t}

let permute (dims:int list) indices : value_node list  =
  let compare_pair (m,_) (n,_) = compare m n in
  let sortedPairs = List.fast_sort compare_pair (List.combine dims indices) in
  List.map snd sortedPairs


let rec idx_or_fixdims
  ~(arr:value_node)
  ~(dims:value_nodes)
  ~(indices:value_nodes) : value_node =
  let nIndices = List.length indices in
  IFDEF DEBUG THEN 
    let nDims = List.length dims in
    if nDims <> nIndices then 
      failwith $ Printf.sprintf 
        "[idx_or_fixdims] Mismatch between # of dims (%d) and # of indices(%d)"
        nDims
        nIndices
  ENDIF; 
  let arrT = arr.value_type in
  (* for convenience, treat indexing into scalars as the identity operation *)
  if ImpType.is_scalar arrT then arr
  else if ImpType.rank arrT = nIndices && List.for_all is_const_int dims then
    idx arr (permute (List.map get_const_int dims) indices)
  else fixdims arr dims indices

