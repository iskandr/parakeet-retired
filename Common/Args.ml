open Base

type 'a formal_args = {
  names : string list;
  defaults : (string * 'a) list; 
}

type 'a actual_args = {
  values : 'a list;
  keywords : (string * 'a) list;
}

let of_names names : 'a formal_args = { names = names; defaults = [] } 
let of_values values = { values = values; keywords = [] } 


let apply_to_formal_values f formals =
  { 
    names = formals.names; 
    defaults = List.map (fun (k,v) -> (k, f v)) formals.defaults;
  } 
 
let apply_to_actual_values f actuals = 
  { 
    values = List.map f actuals.values; 
    keywords = List.map (fun (k,v) -> (k, f v)) actuals.keywords;
  }
let formal_args_to_str ~value_to_str {names; defaults} =
  let defStrings = 
    List.map 
      (fun (k,v) -> 
        k ^ "=" ^ (value_to_str v)) 
    defaults
  in 
  String.concat ", " (names @ defStrings)

let actual_args_to_str ~value_to_str {values; keywords} =
  let posStrings = List.map value_to_str values in 
  let kwdStrings = 
    List.map 
      (fun (k,v) -> k ^ "=" ^ (value_to_str v)) 
      keywords
  in 
  String.concat ", " (List.map value_to_str values @ kwdStrings)

let all_formal_names { names; defaults } = 
  (List.map fst defaults) @ names 

let all_actual_values  { values; keywords } = 
  (List.map snd keywords) @ values 
  

let rec combine_positional env xs ys = 
  match xs, ys with 
   | [], [] -> env
   | x::xs', y::ys' -> combine_positional ((x,y)::env) xs' ys'
   | [], _ -> failwith "Too many arguments" 
   | _, [] -> failwith "Too few arguments" 
     
(* make sure that keywords get placed in the same order each time *) 
let rec bind ?(env=[]) (formals:'a formal_args) (actuals: 'a actual_args) = 
  match formals.defaults with 
  | [] -> combine_positional env formals.names actuals.values
  | (k, d)::rest ->
    let env' = 
      if List.mem_assoc k actuals.keywords then 
        (k, List.assoc k actuals.keywords)::env 
      else 
        (k, d)::env 
    in     
    let actuals' = { 
      values = actuals.values; 
      keywords = List.filter (fun (k', _) -> k' <> k) actuals.keywords;
    }
    in                
    let formals' = {
      names = List.filter ((<>) k) formals.names; 
      defaults = rest; 
    }
    in 
    bind ~env:env' formals' actuals' 
    
    
