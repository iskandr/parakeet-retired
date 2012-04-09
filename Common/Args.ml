open Base

type 'a formal_args = {
  pos_names : string list;
  defaults : (string * 'a) list; 
}

type 'a actual_args = {
  pos_values : 'a list;
  keyword_values : (string * 'a) list;
}

let formal_args_to_str 
  ?(value_to_str = fun _ -> "<default>")
  {pos_names; defaults} = 
  let defStrings = 
    List.map (fun (k,v) -> k ^ "=" ^ (value_to_str v)) defaults
  in 
  String.concat ", " (pos_names @ defStrings)

let actual_args_to_str 
  ?(value_to_str = fun _ -> "<value>")
  {pos_values; keyword_values} = 
  let posStrings = List.map value_to_str pos_values in 
  let kwdStrings = 
    List.map 
      (fun (k,v) -> k ^ "=" ^ (value_to_str v)) 
      keyword_values
  in 
  String.concat ", " (posStrings @ kwdStrings)

let all_formal_names { pos_names; defaults } = 
  pos_names @ (List.map fst defaults)

let all_actual_values  { pos_values; keyword_values } = 
  pos_values @ (List.map snd keyword_values) 
  

let rec combine_positional env xs ys = 
  match xs, ys with 
   | [], [] -> env
   | x::xs', y::ys' -> 
     let env' = String.Map.add x y env in 
     combine_positional env' xs' ys'
   | [], _ -> failwith "Too many arguments" 
   | _, [] -> failwith "Too few arguments" 

let rec bind ?(env=String.Map.empty) formals actuals = 
  match actuals.keyword_values with 
  | [] -> 
    let env' = 
      List.fold_left 
        (fun acc (k, v) -> String.Map.add k v acc) env formals.defaults 
    in
    combine_positional env' formals.pos_names actuals.pos_values
  | (k,v)::rest -> 
    let env' = String.Map.add k v env in 
    (* slightly inefficient but for clarity and avoiding edge cases, 
       remove name from both the positional args *)
    let pos' = List.filter ((<>) k) formals.pos_names in 
    let defaults' = List.remove_assoc k formals.defaults in 
    let formals' = {
      pos_names = List.filter ((<>) k) formals.pos_names; 
      defaults = List.remove_assoc k formals.defaults
    }
    in 
    bind ~env:env' formals' rest 
    
    
