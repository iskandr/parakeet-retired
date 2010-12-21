type t = { inputs: DynType.t list; outputs: DynType.t list option } 

let input_types s = s.inputs

(* create a signature where we know only the input types *)  
let from_input_types types = 
  { inputs = types types; outputs = None } 

let from_types inTypes outTypes = 
  { inputs = inTypes; outputs = Some outTypes } 

let has_output_types s = s.outputs <> None 
                
let output_types s = match s.outputs with 
  | Some ts -> ts
  | None -> failwith "no output types in this signature" 

let to_str signature =
  let inputStr = DynType.type_list_to_str signature.inputs in 
  match signature.outputs with 
      | None -> inputStr
      | Some outputs -> inputStr ^ " -> " ^ (DynType.type_list_to_str outputs)
  