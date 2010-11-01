
open Base

type t = (SSA.FnId.t, SSA.fundef) Hashtbl.t

let add fundef cache =
  let id = fundef.SSA.fn_id in 
  Hashtbl.add cache id fundef; 
  id
  
let find id cache = Hashtbl.find cache id  
let find_option id cache = 
  if Hashtbl.mem cache id  then Some (Hashtbl.find cache id) else None 
  
let mem id cache = Hashtbl.mem cache id 

let from_list (fns :  SSA.fundef list) : t = 
  let h = Hashtbl.create (2 * (List.length fns) + 1) in 
  List.iter (fun fundef -> Hashtbl.add h fundef.SSA.fn_id fundef) fns;
  h

let create (n : int) : t = Hashtbl.create n 

let get_fundef fnTable valNode = match valNode.SSA.value with 
  | SSA.Lam fundef -> fundef 
  | SSA.GlobalFn fnId -> find fnId fnTable
  | _ -> failwith "Expected either local lambda or global function id"