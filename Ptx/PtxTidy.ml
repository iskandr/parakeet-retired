(* pp: -parser o pa_macro.cmo *)

open Ptx
open PtxVal 

(* does this PTX operation assign a value to a register? *)
let is_ptx_assignment = function
  | Bra _
  | Comment _ 
  | Exit 
  | Bar _
  | St _ -> false 
  | _ -> true  

(* which registers serve as inputs to a ptx statement? *) 
let ptx_registers_used instr = 
  let argvals = 
    match instr.op with 
      | St _  -> Array.to_list instr.args
      | _  when Array.length instr.args > 0 ->
         List.tl (Array.to_list instr.args)
      | _ -> []   
  in 
  let get_ids acc = function Sym {id=id} -> id::acc | _ -> acc in 
  let argregs = 
    List.fold_left get_ids [] argvals
  in 
  let predregs = match instr.pred with 
    | IfFalse (Sym {id=id})
    | IfTrue (Sym {id=id}) -> [id]
    | _ -> [] 
  in predregs @ argregs  


let is_live_stmt counts instr = 
  instr.label <> None || 
  instr.pred <> NoGuard || 
  not (is_ptx_assignment instr.op) || 
  match instr.args.(0) with
  | Sym {id=id; space=REG} -> PMap.mem id counts && PMap.find id counts > 0 
  | _ -> true    
  
let count_uniq lst = 
  let rec aux counts = function 
    | [] -> counts
    | x::xs -> 
      let counts' = 
        if PMap.mem x counts then
          PMap.add x (PMap.find x counts + 1) counts 
        else
          PMap.add x 1 counts 
      in aux counts' xs 
  in aux PMap.empty lst 
   
let register_use_counts instructions  = 
  let allUses =
    List.concat (List.map ptx_registers_used (DynArray.to_list instructions)) in
  count_uniq allUses 

    
  (* modifies the DynArray instructions and Hashtbl registerAllocs to 
     get rid of dead registers 
  *) 
let cleanup_kernel instructions registerAllocs = 
  (* keep pruning instructions until code stabilizes *) 
  
  let rec loop iter lastLen =
    (*
    IFDEF DEBUG THEN
      Printf.printf "Running iteration %d of PtxTidy\n" iter;
    ENDIF;
    *)
    let counts = register_use_counts instructions in
    DynArray.filter (is_live_stmt counts) instructions; 
    let currLen = DynArray.length instructions in 
    if currLen <> lastLen  && iter < 100 then loop (iter+1) currLen 
    else counts 
  in 
  let counts = loop 1 (DynArray.length instructions) in  
  let removeDeadAlloc id _ = 
    if not (PMap.mem id counts) || PMap.find id counts < 1 then 
      Hashtbl.remove registerAllocs id
  in  
  Hashtbl.iter removeDeadAlloc registerAllocs
