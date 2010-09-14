    open BaseCommon
    
    module BoolOrd = struct 
      type t = bool
      let compare = compare
    end 
    include BoolOrd 
    module Set = Set.Make(BoolOrd)
    module Map = Map.Make(BoolOrd)
     
    let xor x y = (x || y) && not (x && y)
  
    let add = (||)
    let sub = xor
  
  (* what is division if we only have 0/1? *)
    let mul = (&&) 
    let zero = false
    let one = true 
    let neg = not 
  
    let to_int b = if b then 1 else 0
    let of_int i =  i <> 0
  
    let to_int32 b = if b then Int32.one else Int32.zero
    let of_int32 i = i <> Int32.zero
  
    let to_int64 b = if b then Int64.one else Int64.zero
    let of_int64 i = i <> Int64.zero
  
    let to_float b = if b then 1.0 else 0.0
    let of_float f = f <> 0.0  
  
    let to_string = string_of_bool 
    let of_string = bool_of_string  
