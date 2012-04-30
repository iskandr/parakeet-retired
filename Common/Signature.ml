type t = { 
  inputs: Type.t Args.actual_args; 
  outputs: Type.t list option 
} 

let inputs s = s.inputs

(* create a signature where we know only the input types *)  
let from_input_types ts = { 
  inputs = {Args.values = ts; keywords = []};
  outputs = None; 
}

let from_args args = 
  { inputs = args; outputs = None } 

let with_outputs args outTypes = 
  { inputs = args; outputs = Some outTypes } 

let has_output_types s = s.outputs <> None 
                
let output_types s = match s.outputs with 
  | Some ts -> ts
  | None -> failwith "no output types in this signature" 

let output_types_option s = s.outputs 

let to_str signature =
  let inputStr = 
    Args.actual_args_to_str ~value_to_str:Type.to_str signature.inputs
  in 
  match signature.outputs with 
      | None -> inputStr
      | Some outputs -> inputStr ^ " -> " ^ (Type.type_list_to_str outputs)

