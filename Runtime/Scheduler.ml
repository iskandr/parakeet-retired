(* pp: -parser o pa_macro.cmo *)

(* TODO: Get rid of GPU/Host distinction and replace with arbitrary number*)
(* of backends/memspaces *) 

(* the cost model function expect arguments to be described by triplets  *)
(* of their type, shape, and a boolean indicating whether that argument  *)
(* is on the gpu.                                                        *)

type value = DataId.t Value.t 
type values = value list 


let map ?(axes=[0]) (fn:SSA.fn) ~(fixed:values) (args:values) = assert false 

let reduce ?(axes=[0]) (fn:SSA.fn) ~(fixed:values) ?init (args:values)
    = assert false 

let scan ?(axes=[0]) (fn:SSA.fn)  ~(fixed:values) ?init (args:values) 
    = assert false

let all_pairs ?(axes=[0]) (fn:SSA.fn) ~(fixed:values) (x:value) (y:value) 
    = assert false 

let array_op (op : Prim.array_op) (args : value list) = assert false
