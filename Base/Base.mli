include BaseCommon 

    
module PMap : module type of BasePMap
module List : module type of BaseList 

module Char : sig  
    include Char 
    module Map : module type of BaseMap.Make(Char)
    module Set : module type of BaseSet.Make(Char) 
    module Graph  : module type of Graph.Make(Char) 

    val to_string : char -> string 
end


module Int32 : sig 
    include Int32
    module Map : module type of BaseMap.Make(Int32)
    module Set : module type of BaseSet.Make(Int32)
    module Graph : module type of Graph.Make(Int32) 
    
    val of_bool : bool -> t
    val to_bool : t -> bool
    val of_char : char -> t  
end 

module Int64 : sig
    include Int64
    module Map : module type of BaseMap.Make(Int64)
    module Set : module type of BaseSet.Make(Int64)
    module Graph : module type of Graph.Make(Int64) 
    
    val of_bool : bool -> t
    val to_bool : t -> bool 
    val of_char : char -> c 
end

module String : sig 
  include ExtString.String
  module Map : module type of BaseMap.Make(ExtString.String)
  module Set : module type of BaseSet.Make(ExtString.String)
  module Graph : module type of Graph.Make(ExtString.String) 
  
  val abbrev : string -> int -> string  
end

module Array : module type of BaseArray 
  
module Hashtbl : sig 
    include ExtHashtbl.Hashtbl 
    val of_list : ('a * 'b) list -> ('a, 'b) t 
    val remove_list : ('a, 'b) t -> 'a list -> unit  
    val combine : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
end 
