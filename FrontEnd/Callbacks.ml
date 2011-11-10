(*** FrontEnd ***)
let _ = Callback.register "register_untyped_function"
  FrontEnd.register_untyped_function
let _ = Callback.register "run_function" FrontEnd.run_function

(*** ExternalLibNames ***) 
let _ = Callback.register "get_prim" ExternalLibNames.get_prim


(*** SHAPE ***)
let _ = Callback.register "shape_create" Shape.create
let _ = Callback.register "shape_rank" Shape.rank

(*** AST ***)
let _ = Callback.register "mk_ast_info" AST.mk_ast_info

(*** TYPE ***)
let _ = Callback.register "sizeof_dyn_type" Type.sizeof 
let _ = Callback.register "type_rank" Type.rank
let _ = Callback.register "get_elt_type" Type.elt_type

let _ = Callback.register "bool_elt_t" Type.BoolT
let _ = Callback.register "bool_t" Type.bool
let _ = Callback.register "type_is_bool" Type.is_bool

let _ = Callback.register "char_elt_t" Type.CharT
let _ = Callback.register "char_t" Type.char
let _ = Callback.register "type_is_char" Type.is_char

let _ = Callback.register "int32_elt_t" Type.Int32T
let _ = Callback.register "int32_t" Type.int32
let _ = Callback.register "type_is_int32" Type.is_int32
  
let _ = Callback.register "int64_elt_t" Type.Int64T
let _ = Callback.register "int64_t" Type.int64
let _ = Callback.register "type_is_int64" Type.is_int64

let _ = Callback.register "float32_elt_t" Type.Float32T
let _ = Callback.register "float32_t" Type.float32
let _ = Callback.register "type_is_float32" Type.is_float32
 
let _ = Callback.register "float64_elt_t" Type.Float64T
let _ = Callback.register "float64_t" Type.float64
let _ = Callback.register "type_is_float64" Type.is_float64

(*** Value ***) 
let _ = Callback.register "value_of_bool" Value.of_bool
let _ = Callback.register "value_of_char" Value.of_char
let _ = Callback.register "value_of_int32" Value.of_int32
let _ = Callback.register "value_of_int64" Value.of_int64
let _ = Callback.register "value_of_float64" Value.of_float

let _ = Callback.register "value_to_bool"  Value.to_bool
let _ = Callback.register "value_to_char"  Value.to_char
let _ = Callback.register "value_to_int32"  Value.to_int32
let _ = Callback.register "value_to_int64"  Value.to_int64
let _ = Callback.register "value_to_float64"  Value.to_float

let _ = Callback.register "value_array" Value.mk_array 