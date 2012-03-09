open Base
open Imp


(* HELPER FUNCTIONS FOR IMP EXPRESSIONS *)
let wrap_bool (v : value) : value_node =
  {value=v; value_type = ImpType.bool_t}
let wrap_char (v : value) : value_node =
  {value=v; value_type = ImpType.char_t}
let wrap_int16 (v : value) : value_node =
  {value=v; value_type = ImpType.int16_t}
let wrap_int32 (v : value) : value_node =
  {value=v; value_type = ImpType.int32_t}
let wrap_int64 (v : value) : value_node =
  {value=v; value_type = ImpType.int32_t}
let wrap_float32 (v : value) : value_node =
  {value=v; value_type = ImpType.float32_t}
let wrap_float64 (v : value) : value_node =
  {value=v; value_type = ImpType.float64_t}

let wrap (v:value) (ty:ImpType.t) = {value = v; value_type = ty}

let common_type ?t (args : value_node list) =
 match t with
 | Some t -> ImpType.ScalarT t
 | None ->
   let types = List.map (fun node -> node.value_type) args in
   ImpType.combine_type_list types

let typed_op op ?t (args : value_node list) : value_node =
  let argType = common_type ?t args in
  wrap (Op (argType, op, args)) argType

(* Same as typed_op, but with comparison operators which always return bools *)
let cmp_op op ?t args =
  let argType = common_type ?t args in
  assert (ImpType.is_scalar argType);
  wrap_bool $ Op (argType, op, args)

(* CUDA stuff *)
type vec3 = {x: value_node; y: value_node; z: value_node}
let mk_vec3 (f : coord -> value_node) : vec3  = {x = f X; y = f Y; z = f Z}

let threadIdx =
  mk_vec3 (fun coord -> wrap_int16 $ CudaInfo(ThreadIdx, coord))
let blockIdx =
  mk_vec3 (fun coord -> wrap_int16 $ CudaInfo(BlockIdx, coord))
let blockDim =
  mk_vec3 (fun coord -> wrap_int16 $ CudaInfo(BlockDim, coord))
let gridDim =
  mk_vec3 (fun coord -> wrap_int16 $ CudaInfo(GridDim, coord))

(* GENERAL IMP EXPRESSIONS *)
let int32 i = wrap_int32 $ Const (ParNum.Int32 i)
let int i =  wrap_int32 $ Const (ParNum.Int32 (Int32.of_int i))
let float f = wrap_float32 $ Const (ParNum.Float32 f)
let double d = wrap_float64 $ Const (ParNum.Float64 d)
let bool b = wrap_bool $ Const (ParNum.Bool b)

let zero = int 0
let one = int 1

let ints_til (n:int) : value_node list = List.map int (List.til n)

let select cond t f =
  assert (t.value_type = f.value_type);
  { value = Select(t.value_type, cond, f, f);
    value_type = t.value_type
  }

let dim (arr:value_node) (idx:value_node) = wrap_int32 $ (DimSize(arr, idx))

let len x = dim (int 0) x

let max_no_simplify ?t x y = typed_op Prim.Max ?t [x;y]
let min_no_simplify ?t x y = typed_op Prim.Min ?t [x;y]

let mul_no_simplify ?t x y =
  let xt = x.value_type in
  let yt = y.value_type in
  if ImpType.is_int xt && ImpType.is_int yt then
    match x.value, y.value with
      | Imp.Const n, _
      | _, Imp.Const n when ParNum.is_zero n -> {zero with value_type = xt}
      | _, Imp.Const n when ParNum.is_one n -> x
      | Imp.Const n, _ when ParNum.is_one n -> y
      | _ ->   typed_op Prim.Mult ?t [x;y]
  else typed_op Prim.Mult ?t [x;y]

let max (d1:value_node) (d2:value_node) : value_node =
  if d1.value = d2.value then d1 else max_no_simplify d1 d2

let mul (d1:value_node) (d2:value_node) =
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
  | _ -> mul_no_simplify d1 d2

let add_no_simplify ?t x y = typed_op Prim.Add ?t [x; y]

let add (d1:value_node) (d2:value_node) : value_node =
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
  | _ -> add_no_simplify d1 d2

let div x y = typed_op Prim.Div [x; y]
let sub x y = typed_op Prim.Sub [x; y]

let ( *$ ) = mul
let ( +$ ) = add
let ( /$ ) = div
let ( -$ ) = sub

let mod_ ?t x y = typed_op Prim.Mod ?t [x; y]
let ( %$ ) = mod_

let safe_div_ ?t x y = typed_op Prim.SafeDiv ?t [x;y]

let lt ?t x y = cmp_op Prim.Lt ?t [x; y]
let ( <$ ) = lt

let lte ?t x y = cmp_op Prim.Lte ?t [x; y]
let ( <=$ ) = lte

let gt ?t x y = cmp_op Prim.Gt ?t [x;y]
let ( >$ ) = gt

let gte ?t x y = cmp_op Prim.Gte ?t [x;y]
let ( >=$ ) = gte

let eq ?t x y = cmp_op Prim.Eq ?t [x;y]
let ( =$ ) = eq

let neq ?t x y = cmp_op Prim.Neq ?t [x;y]
let ( <>$ ) = neq

let not_ x = typed_op Prim.Not ~t:Type.BoolT [x]
let ( !$ ) = not_

let and_ x y = typed_op Prim.And ~t:Type.BoolT [x;y]
let ( &&$ ) = and_

let or_ x y = typed_op Prim.Or ~t:Type.BoolT [x;y]
let ( ||$ ) = or_

let sqrt32 x = typed_op Prim.Sqrt ~t:Type.Float32T [x]
let sqrt64 x = typed_op Prim.Sqrt ~t:Type.Float64T [x]

let ln_32 x = typed_op Prim.Ln ~t:Type.Float32T [x]
let ln_64 x = typed_op Prim.Ln ~t:Type.Float64T [x]

let id_of_value valNode = match valNode.value with
  | Var id -> id
  | _ -> assert false

let var ~ty id = {value = Var id; value_type = ty}


let vec_slice arr width idx =
  let arrT = arr.value_type in
  if ImpType.is_scalar arrT then failwith "Can't VecSlice into a scalar"
  else begin
    assert (ImpType.is_array arrT);
    let rank = ImpType.rank arrT in
    if rank <> 1 then
      failwith $ Printf.sprintf
        "[ImpHelpers] VecSlice expected rank 1 array, got %d"
        rank
    ;
    let eltT = ImpType.elt_type arrT in
    {
      value = VecSlice(arr, idx, width);
      value_type = ImpType.VectorT(eltT, width)
    }
  end

let cmp cmpOp x y =
  assert (x.value_type = y.value_type);
  {
    value = Imp.Op(x.value_type, cmpOp,[x;y]);
    value_type = ImpType.bool_t
  }

let scalar_op op x y =
  assert (x.value_type = y.value_type);
  {
    value = Imp.Op(x.value_type, op, [x;y]);
    value_type = x.value_type
  }



let is_const_int {value} = match value with
  | Const n -> ParNum.is_int n
  | _ -> false

let get_const_int {value} = match value with
  | Const n -> ParNum.to_int n
  | _ -> failwith "Not an integer!"

let fixdim ~arr ~dim ~idx : value_node  =
  {
    value = FixDim(arr, int dim, idx);
    value_type = ImpType.peel ~num_axes:1 arr.value_type
  }

let permute (dims:int list) indices : value_node list  =
  let compare_pair (m,_) (n,_) = compare m n in
  let sortedPairs = List.fast_sort compare_pair (List.combine dims indices) in
  List.map snd sortedPairs


(* assume indices are into sequential dims *)
let idx arr ?dims indices =
  let arrT = arr.value_type in
  let rank = ImpType.rank arrT in
  (* for convenience, treat indexing into scalars as the identity operation *)
  if rank = 0 then arr
  else begin
    let numIndices = List.length indices in
    if numIndices <> rank then
      failwith $ Printf.sprintf
        "[ImpHelpers] Expected %d indices, got %d"
        rank
        numIndices
    ;
    let eltT = ImpType.elt_type arrT in
    (* assume dims are constant or implicitly 0..n-1 *)
    let indices' = match dims with
      | Some dims ->  permute dims indices
      | None -> indices
    in
    {value = Idx(arr, indices); value_type = ImpType.ScalarT eltT }
  end


let slice ~arr ~dim ~start ~stop =
  { value = Slice(arr, dim, start, stop);
    value_type = arr.value_type
  }


