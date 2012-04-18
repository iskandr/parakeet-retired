open Base

type ('a, 'b) formal_args = {
  names : 'a list;
  defaults : ('a * 'b) list; 
}

type ('a, 'b) actual_args = {
  values : 'b list;
  keywords : ('a * 'b) list;
}

let of_names names = { names = names; defaults = [] } 
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
  String.concat ", " (List.map name_to_str names @ defStrings)

let actual_args_to_str ~value_to_str {values; keywords} =
  let posStrings = List.map value_to_str values in 
  let kwdStrings = 
    List.map 
      (fun (k,v) -> k ^ "=" ^ (value_to_str v)) 
      keywords
  in 
  String.concat ", " (List.map value_to_str values @ kwdStrings)

let all_formal_names { names; defaults } = 
  names @ (List.map fst defaults)

let all_actual_values  { values; keywords } = 
  values @ (List.map snd keywords) 
  

let rec combine_positional env xs ys = 
  match xs, ys with 
   | [], [] -> env
   | x::xs', y::ys' -> combine_positional ((x,y)::env) xs' ys'
   | [], _ -> failwith "Too many arguments" 
   | _, [] -> failwith "Too few arguments" 

let rec bind ?(env=[]) formals actuals = 
  match actuals.keywords with 
  | [] -> 
    let env' =  
      List.fold_left (fun lst pair -> pair::lst) env formals.defaults     in
    combine_positional env' formals.names actuals.values
  | (k,v)::rest -> 
    let env' = (k,v)::env in  
    (* slightly inefficient but for clarity and avoiding edge cases, 
       remove name from both the positional args *)
    let formals' = {
      names = List.filter ((<>) k) formals.names; 
      defaults = List.remove_assoc k formals.defaults
    }
    in 
    bind ~env:env' formals' { actuals with keywords = rest } 
    
    
