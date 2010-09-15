
type t = { bits: Int32.t; float64: float} 


external bits32_of_float : float -> Int32.t = "bits32_of_float64"
external float_of_bits32 : Int32.t -> float = "float64_of_bits32"

let to_float f32 = f32.float64 
let to_bits f32 = f32.bits
let to_bits64 f32 = Int64.of_int32 (to_bits f32)

let of_float f64 = { float64= f64; bits= bits32_of_float f64}
let of_bits bits = { float64 = float_of_bits32 bits; bits = bits }  
let of_bits64 bits =
  let bits32 = Int64.to_int32 bits in 
  { float64 = float_of_bits32 bits32; bits = bits32 }
  
let compare x y = compare x.float64 y.float64 
let equiv x y = Float.equiv x.float64 y.float64 

let test x =
  let x' = to_float (of_bits (to_bits (of_float x))) in   
  if not (Float.equiv x x') then 
  failwith 
     (Printf.sprintf "[Float32] expected %f, but got %f. Diff: %f" x x' (x-.x'))    
     