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

let peel_vec_elt = function 
  | Type (DynType.VecT t) -> Type t
  | Type t when DynType.is_scalar t -> Type t 
  | _ -> failwith "[signature->peel_vec_elt] expected vector type"  

let peel_vec_types signature = 
  assert (List.for_all DynType.is_scalar_or_vec (input_types signature)); 
  { signature with inputs = List.map peel_vec_elt signature.inputs }   
 

          