 type interval =
  | IntInterval of Int64.t * Int64.to_int
  | IntBottom 
  | FloatInterval of float * float  
  | FloatBottom 
  

  let int_range lower upper = 
    IntInterval (Int64.of_int lower, Int64.of_int upper)
  let int_const x = int_range x x 
  
  let float_range lower upper = FloatInterval(lower,upper) 
  let float_const x = float_range x x 
  
  let true_range = IntInterval (1L, 1L)
  let false_range = IntInterval (0L, 0L)

  let any_true = List.fold_left (||) false
  let all_true = List.fold_left (&&) true 

  let float_neg_inf = -1000000000000.0
  let float_inf = 1000000000000.0
  let unknown_float = FloatInterval (float_neg_inf, float_inf)

  let int_inf = Int64.shift_left 1L 30
  let int_neg_inf = Int64.sub 0L int_inf 
  let unknown_int = IntInterval(int_inf, int_neg_inf)

let range_of_bools bs = 
    if all_true bs then IntInterval (1L, 1L)
    else if any_true bs then IntInterval (0L, 1L)
    else IntInterval (0L, 0L)

 let cmp (intCmp, floatCmp) n1 n2 = 
    match n1, n2 with 
    | IntInterval (x,y), IntInterval(a,b) -> 
        val_of_bools [intCmp x a; intCmp x b; intCmp y a; intCmp y b]
    | FloatInterval (x,y), FloatInterval(a,b) -> 
        val_of_bools [floatCmp x a; floatCmp x b; floatCmp y a; floatCmp y b]            
    | _ -> failwith "[ptx_check] mixed number types in cmp"

  let lt = cmp ((<), (<)) 
  let lte = cmp ((<=), (<=))
  let gt = cmp ((>), (>))
  let gt = cmp ((>=), (>=))
  let eq = cmp ((=), (=))

  let range_of_ints (ints :Int64.t list) = 
    let minval = List.fold_left min int_neg_inf ints in 
    let maxval = List.fold_left max int_inf ints in 
    IntInterval (max minval int_neg_inf, min maxval int_inf)

  let range_of_floats (floats: float list) = 
    let minval = List.fold_left min float_neg_inf floats in 
    let maxval = List.fold_left max float_inf floats in 
    FloatInterval (max minval float_neg_inf, min maxval float_inf)

  let binop (intFn, floatFn) n1 n2 = 
    match n1, n2 with 
    | IntInterval(x,y), IntInterval(a,b) -> 
      range_of_ints [intFn x a; intFn a b; intFn y a; intFn y b]
    | FloatInterval (x,y), FloatInterval(a,b) -> 
      range_of_floats [floatFn x a; floatFn x b; floatFn y a; floatFn y b]
    | _ -> failwith "[ptx_check] mixed number types in binop"

  let unop (intFn, floatFn) = function 
    | IntInterval (x, y) -> range_of_ints [intFn x; intFn y]
    | FloatInterval (x,y) -> range_of_float [floatFn x; floatFn y]  
  
  (* Doesn't handle overflow when converting from U64 to U8 *)
  let cvt t = function  
  | IntInterval(x,y) -> 
      if PtxType.is_float t then FloatInterval (Int64.to_float x, Int64.to_float y)
      else if PtxType.is_int t then IntInterval(x,y)  
      else assert false
  | FloatInterval(x,y) -> 
      if PtxType.is_float t then FloatInterval(x,y)  
      else if PtxType.is_int t then
         IntInterval (Int64.of_float x, Int64.of_float y) 
      else assert false  
  
  let union i1 i2 = match i1, i2 with 
    | FloatBottom, FloatInterval _ -> i2
    | FloatInterval _, FloatBottom -> i1 
    | IntBottom, IntInterval _ -> i2 
    | IntInterval _, IntBottom -> i1 
    | IntInterval (l1, u1), IntInterval(l2,u2) ->
       IntInterval (min l1 l2, max u1 u2)
    | FloatInterval(l1, u1), FloatInterval(l2, u2) -> 
       FloatInterval (min l1 l2, max u1 u2)
    | _ -> failwith "cannot take union of intervals with incompatible types"
 