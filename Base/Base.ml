(* pp: -parser o pa_macro.cmo *)

include BaseCommon

let _ = Printexc.record_backtrace true 
    
module PMap = BasePMap
module List = BaseList
module Map = BaseMap  
module Set = BaseSet 

(* extensions to existing modules go here, whereas entirely new modules
   get their own files
*) 
module Char = struct 
    include Char 
    module Map = BaseMap.Make(Char)
    module Set = BaseSet.Make(Char) 
    module Graph = Graph.Make(Char) 

    let to_string = Printf.sprintf "%c"
end

module Int32 = struct 
    include Int32
    module Map = BaseMap.Make(Int32)
    module Set = BaseSet.Make(Int32)
    module Graph = Graph.Make(Int32) 
    
    let of_bool b = if b then Int32.one else Int32.zero
    let to_bool i = (i > Int32.zero)
    
    let of_char c = of_int $ Char.code c 
end 

module Int64 = struct
    include Int64
    module Map = BaseMap.Make(Int64)
    module Set = BaseSet.Make(Int64)
    module Graph = Graph.Make(Int64) 
    
    let of_bool b = if b then Int64.one else Int64.zero
    let to_bool i = (i > Int64.zero)
    
    let of_char c = of_int $ Char.code c
end


module String = struct 
  include ExtString.String
  module Map = BaseMap.Make(ExtString.String)
  module Set = BaseSet.Make(ExtString.String)
  module Graph = Graph.Make(ExtString.String) 
  
  let abbrev s n = 
    if length s > n then 
      let s' = sub s 0 (n-3) in 
      s' ^ "..."
    else s 
end

module Array = BaseArray 
  
module Hashtbl = struct 
    include ExtHashtbl.Hashtbl 
    
    IFDEF DEBUG THEN
      (* makes lookups slightly slower, 
         so only use the descriptive exception in debug mode 
       *) 
      exception KeyNotFound of string 
      let find hash x = 
        try find hash x  with _ -> raise (KeyNotFound (dump x))
    ENDIF

    let of_list pairs = 
        let hash  = Hashtbl.create (List.length pairs) in 
        let iter_fn (a,b) = Hashtbl.add hash a b in 
        List.iter iter_fn pairs; hash
            
    let remove_list hash keys = List.iter (Hashtbl.remove hash) keys 
    
    let combine h1 h2 = 
      let h3 = create (2*(length h1 + length h2) + 1) in 
      Hashtbl.iter (add h3) h1;  
      Hashtbl.iter (add h3) h2; 
      h3               
end 
