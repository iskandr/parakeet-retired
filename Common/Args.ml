

type 'a formal_args = {
  pos_names : string list;
  defaults : (string * 'a) list; 
}

type 'a actual_args = {
  pos_values : 'a list;
  keyword_values : (string * 'a) list;
}

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
    let env = 
      List.fold_left 
        (fun acc (k, v) -> String.Map.add k v acc) env formals.defaults 
    in
    combine_positional env formals.pos_names actuals.pos_values
  | (k,v)::rest -> 
    let env' = String.Map.add k v env in 
    (* slightly inefficient but for clarity and avoiding edge cases, 
       remove name from both the positional args 
    let pos' = List.filter ((<>) k) formals.pos_names in 
    let defaults' 
      if List.mem k formals.pos_names then 
        List.rem k formals.pos_names, formals.default_
       
    if List.mem_assoc k formals.p
    
    
