(* specialized versions of functions can be specialized on either the type *)
(* or the literal value of their inputs,*)
(* and optionally on types of outputs *) 

type input_elt = Value of SSA.value | Type of DynType.t
type t = { inputs : input_elt list; outputs : DynType.t list option } 

let input_elt_to_type = function 
  | Type t -> t 
  | _ -> failwith "type expected in signature"

let input_types s = List.map input_elt_to_type s.inputs

(* create a signature where we know only the input types *)  
let from_input_types types = 
  { inputs = List.map (fun t -> Type t) types; outputs = None } 

let from_types inTypes outTypes = 
  { inputs = List.map (fun t -> Type t) inTypes; outputs = Some outTypes } 

let mk_sig inTypes ?outTypes = 
  { inputs = List.map (fun t -> Type t) inTypes; outputs = outTypes } 

        
let output_types s = match s.outputs with 
  | Some ts -> ts
  | None -> failwith "no output types in this signature" 

let all_scalar_types s = 
  List.for_all (function Type t ->  DynType.is_scalar t | _ -> false) s.inputs 

          