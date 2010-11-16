(* specialized versions of functions can be specialized 
   on either the type or the literal value of their inputs,
   and optionally on types of outputs. 
   Function values need to also be specialized on the types of 
   any partially applied arguments.  
  
*) 


type sig_elt =
  | Type of DynType.t 
  | Const of PQNum.num 
  | Closure of SSA.value * sig_elt list   
 
type t = { inputs : sig_elt list; outputs : DynType.t list option } 

let sig_elt_to_type = function 
  | Type t -> t 
  | _ -> failwith "type expected in signature"

let sig_elts_to_types = List.map sig_elt_to_type 

let input_types s = List.map sig_elt_to_type s.inputs

(* create a signature where we know only the input types *)  
let from_input_types types = 
  { inputs = List.map (fun t -> Type t) types; outputs = None } 

let from_types inTypes outTypes = 
  { inputs = List.map (fun t -> Type t) inTypes; outputs = Some outTypes } 

let mk_sig ?outTypes  inTypes = 
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
 
(*
let rec sig_from_values = function 
  | [] -> []
  | vNode::rest ->
      let curr =  
        if DynType.is_function vNode.SSA.value_type then Value vNode.SSA.value
        else Type vNode.SSA.value_type
      in curr :: (sig_from_values rest)  
*)      
let rec sig_elt_to_str = function 
  | Type t -> DynType.to_str t 
  | Const n -> Printf.sprintf "Const (%s)" (PQNum.num_to_str n)
  | Closure (v, args) -> 
      let closureArgStr = sig_elts_to_str args in
      let fnStr = Printf.sprintf "Closure(%s)" (SSA.value_to_str v) in 
      if closureArgStr = "[]" then fnStr 
      else fnStr ^ closureArgStr
and sig_elts_to_str elts = 
  "["^ (String.concat ", " (List.map sig_elt_to_str elts)) ^ "]" 

let to_str signature =
  let inputStr = sig_elts_to_str signature.inputs in 
  match signature.outputs with 
      | None -> inputStr
      | Some outputs -> inputStr ^ " -> " ^ (DynType.type_list_to_str outputs)
  