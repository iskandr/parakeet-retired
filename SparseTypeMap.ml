open Base 
(* sparse type envrionment which doesn't store BottomT *) 

type 'a t = ('a, DynType.t) PMap.t 

let empty = PMap.empty 

let find id env = 
  if PMap.mem id env then PMap.find id env else DynType.BottomT
  
let add id t env = 
  if t = DynType.BottomT then env
  else PMap.add id t env 

let from_list lst = PMap.of_enum (List.enum lst) 
