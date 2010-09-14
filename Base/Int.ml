open BaseCommon
                                 
    module IntOrd = struct 
      type t = int      
      let compare = compare
    end
    include IntOrd 
    module Map = BaseMap.Make(IntOrd)
    module Set = BaseSet.Make(IntOrd) 
    module Graph = Graph.Make(IntOrd)
     
    let add = (+)
    let sub = (-)
    let mul = ( * )
    let div = ( / ) 
    let zero = 0
    let one = 1 
    let neg x = - x
    let succ x = x + 1
    let pred x = x - 1
  
    let of_float = int_of_float
    let to_float = float_of_int
   
    let to_string = string_of_int 
    let from_string = int_of_string
  
    let of_bool b =  if b then 1 else 0 
    let to_bool i = i <> 0 
    
    let of_char c = Char.code c 
