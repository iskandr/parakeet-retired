    
    module FloatOrd = struct 
      type t = float 
      let compare = compare
    end
    
    module Map = BaseMap.Make(FloatOrd)
    module Set = BaseSet.Make(FloatOrd)
    module Graph = Graph.Make(FloatOrd) 
    
    (* somewhat unreliable approximate float equality *) 
    let equiv x y = 
       (x = y) ||
       let eps = 0.0000001 in 
       let diff = abs_float(x -. y) in   
        diff <= eps || 
        diff <= (max (abs_float x) (abs_float y)) *. eps 
        
      
    let add = (+.)
    let sub = (-.)
    let mul = ( *. )
    let div = ( /. )
    let zero = 0.0
    let one = 1.0 
    let neg x = -. x
    let succ x = x +. 1.0
    let pred x = x -. 1.0
  
    let to_int = int_of_float
    let of_int = float_of_int
  
    let of_bool b = if b then 1.0 else 0.0
    let to_bool f = f <> 0.0 
   
    let to_string f = string_of_float f
    let of_string s = float_of_string s 
