open Base

type 'a array_info = {
  data : 'a;
  array_type : Type.t;
  elt_type : Type.elt_t;
  array_shape : Shape.t;
  array_strides : int array;
}

type 'a t =
  | Array of 'a array_info
  | Scalar of ParNum.t
  | Explode of ParNum.t * Shape.t         (* scalar, shape *)
  | Rotate of 'a t * int * int            (* array, dim, offset *)
  | Shift of 'a t * int * int * ParNum.t  (* array, dim, offset, default *)
  | FixDim of 'a t * int * int            (* array, dim, idx *)
  | Slice of 'a t * int * int * int       (* array, dim, start, end *)
  | Range of int * int * int              (* start, stop, step *)
  | NoneVal
  | Nested of 'a t array

(* since array data is polymorphic it's by default printed as the *)
(* totally uninformative string '<array>'. If you want something more*)
(* specific than that, you'll have to supply your own array printing *)
(* function. *)
let rec to_str ?(array_to_str=(fun _ -> "<array>")) = function
  | Scalar n -> (ParNum.to_str n) ^ (Type.elt_to_short_str (ParNum.type_of n))
  | Array array_info -> array_to_str array_info
  | Explode (n, s) ->
    Printf.sprintf "explode(%s, %s)" (ParNum.to_str n) (Shape.to_str s)
  | Rotate (a, dim, offset) ->
    Printf.sprintf "rotate(%s, dim=%d, offset=%d)"
      (to_str ~array_to_str a) dim offset
  | Shift (a, dim, offset, default) ->
    Printf.sprintf "rotate(%s, dim=%d, offset=%d, default=%s)"
      (to_str ~array_to_str a) dim offset (ParNum.to_str default)
  | FixDim(a, dim, idx) ->
    Printf.sprintf "fixdim(%s, dim=%d, idx=%d)"
      (to_str ~array_to_str a)  dim idx
  | Slice (a, dim, start, stop) ->
    Printf.sprintf "slice(%s, dim=%d, start=%d, stop=%d)"
      (to_str ~array_to_str a)  dim start stop
  | Range (start, stop, step) ->
    Printf.sprintf "range(from=%d, to=%d, step=%d)" start stop step
  | NoneVal -> "none"
  | Nested elts -> "nested array"

let list_to_str ?(array_to_str=(fun _ -> "<array>")) vals =
  String.concat ", " (List.map (to_str ~array_to_str) vals)

let generic_array_to_str {elt_type; array_shape} : string =
  Printf.sprintf "<array> : %s (shape=%s)"
    (Type.elt_to_str elt_type)
    (Shape.to_str array_shape)

let rec map (f: 'a -> 'b) (x : 'a t) : 'b t = match x with
  | Array array_info -> Array {array_info with data = f array_info.data}
  (*| Nested elts -> Nested (Array.map (map f) elts)*)
  | Rotate (a, dim, offset) -> Rotate (map f a, dim, offset)
  | Shift (a, dim, offset, default) -> Shift (map f a, dim, offset, default)
  | FixDim (a, dim, idx) -> FixDim (map f a, dim,  idx)
  | Slice (a, dim, start, stop) -> Slice (map f a, dim, start, stop)
  | Range (start, stop, step) -> Range (start, stop, step)
  | Explode (n, s) -> Explode (n, s)
  | Scalar n -> Scalar n
  | NoneVal -> NoneVal
  | Nested elts -> Nested (Array.map (map f) elts)


let rec get_underlying_array = function
  | Array a -> a
  | Shift (a, _, _, _)
  | FixDim(a, _, _)
  | Slice (a, _, _, _)
  | Rotate (a, _, _) -> get_underlying_array a
  | Scalar n -> failwith "Unable to get underlying array for scalar."
  | Explode (n, s) -> failwith "Don't know how to handle explodes yet."
  | Range _ -> failwith "Don't know how to handle ranges yet."
  | Nested _
  | NoneVal -> assert false


let rec elt_type = function
  | Explode (n, _)
  | Scalar n -> ParNum.type_of n
  | Range _ -> Type.Int32T
  | NoneVal -> failwith  "[Value] Null has no element type"
  | Nested  _ ->
    failwith
      "[Value] Nested array not guaranteed to have a single element type"
  | other -> Type.elt_type $ (get_underlying_array other).array_type

let rec shape_of = function
  | Array {array_shape} -> array_shape
  | Scalar n -> Shape.scalar_shape
  | Rotate (x, _, _)
  | Shift (x, _, _, _) -> shape_of x
  | FixDim(x, dim, idx) ->
    let originalShape = shape_of x in
    Shape.slice_shape originalShape [dim]
  | Slice (x, dim, start, stop) ->
    let dims = Shape.to_array (shape_of x) in
    dims.(dim) <- stop - start + 1;
    Shape.of_array dims
  | Range (start, stop, step) -> Shape.of_list [(stop - start + 1)/step]
  | Explode _
  | NoneVal -> failwith "Value has no shape"
  | Nested elts ->
    let nelts = Array.length elts in
    if nelts = 0 then Shape.scalar_shape else
      let shapes = Array.map shape_of elts in
      let firstShape = shapes.(0) in
      Shape.append_dim nelts firstShape

let rank v = Shape.rank  (shape_of v)

let fixdim dim idx v =
  if rank v = 0 then v
  else FixDim(v, dim, idx)


let rec type_of v =
  Printf.printf "[ocaml value.type_of] %s \n%!" (to_str v);
  let t = match v with
  | Array {array_type} -> array_type
  (*| Nested _ -> failwith "nested arrays not supported"*)
  | Scalar n -> Type.ScalarT (ParNum.type_of n)
  | Explode (n, s) -> Type.ArrayT (ParNum.type_of n, Shape.rank s)
  | FixDim(x, _, _) ->
    let nestedT = type_of x in
    let rank = Type.rank nestedT in
    let eltT = Type.elt_type nestedT in
    Type.ArrayT (eltT, rank)
  | Shift (x, _, _, _)
  | Slice (x, _, _, _)
  | Rotate (x, _, _) -> type_of x
  | Range _ -> Type.ArrayT(Type.Int32T, 1)
  | Nested _ -> Type.AnyT 
  | NoneVal -> Type.NoneT
  in
  Printf.printf "[ocaml value.type_of result] %s\n%!" (Type.to_str t);
  t

let type_of_list vals = List.map type_of vals

let get_shape = shape_of
let get_strides = function
  | Array {array_strides} -> array_strides
  | _ -> failwith "Cannot get strides of non-array"

let to_num = function
  | Scalar n -> n
  | other -> failwith $ Printf.sprintf
                 "Can't get scalar from interpreter value: %s"
                 (to_str other)

let to_bool x = ParNum.to_bool (to_num x)
let to_char x = ParNum.to_char (to_num x)
let to_int x = ParNum.to_int (to_num x)
let to_int32 x = ParNum.to_int32 (to_num x)
let to_int64 x = ParNum.to_int64 (to_num x)
let to_float x = ParNum.to_float (to_num x)

let mk_none () = NoneVal 
let of_num n = Scalar n
let of_bool b = of_num (ParNum.of_bool b)
let of_char c = of_num (ParNum.of_char c)
let of_int i = of_num (ParNum.of_int i)
let of_float32 f = of_num (ParNum.of_float32 f)
let of_float f = of_num (ParNum.of_float f)
let of_int32 i32 = of_num (ParNum.of_int32 i32)
let of_int64 i64 = of_num (ParNum.of_int64 i64)

let mk_array (data:'a) (elt_t:Type.elt_t) (shape:Shape.t) (strides:int array) =
  let ty = Type.ArrayT(elt_t, Shape.rank shape) in
  Array {
    data = data;
    array_type = ty;
    elt_type = elt_t;
    array_shape = shape;
    array_strides = strides;
  }

let is_scalar x =
  Printf.printf "[ocaml is_scalar] Getting type...\n%!";
  let t = type_of x in
  Printf.printf "[ocaml is_scalar] Checking whether is scalar...\n%!";
  Type.is_scalar t

let rec extract = function
  | Rotate (x, _, _)
  | Shift (x, _, _, _)
  | FixDim (x, _ , _)
  | Slice (x, _, _, _) -> extract x
  | Array {data} -> Some data
  | NoneVal 
  | Nested _
  | Scalar _
  | Explode _
  | Range _ -> None

let rec collect_list = function
  | [] -> []
  | x::xs ->
    let rest = collect_list xs in
    (match extract x with None -> rest | Some d -> d :: rest)
