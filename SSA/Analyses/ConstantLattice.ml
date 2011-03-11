 
type 'a t = Const of 'a | NoValue | ManyValues 
let top = ManyValues
let bottom = NoValue
let join v1 v2 = match v1, v2 with 
  | NoValue, NoValue -> NoValue 
  | Const _, NoValue -> v1 
  | NoValue, Const _ -> v2
  | ManyValues, _
  | _, ManyValues -> ManyValues
  | Const c1, Const c2 -> if c1 = c2 then v1 else ManyValues
  
let const_to_str = function 
  | NoValue -> "<no info>"
  | ManyValues -> "<multiple values>"
  | Const _ -> "<constant>"  

let extract_value = function  
  | Const v -> v 
  | _ -> failwith "expected constant"
 