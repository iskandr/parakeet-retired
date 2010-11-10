open Base 
open SSA

type optimization = FnTable.t -> SSA.fundef -> SSA.fundef * bool   

let rec fold_optimizations fnTable code lastChanged = function
  | (name, opt)::rest -> 
        
      let code', changed = opt fnTable code in
(*      debug $ Printf.sprintf "[optimizer] Ran %s ...%s\n"
        name (if changed then "MODIFIED" else "");*)
      fold_optimizations fnTable code' (changed || lastChanged)  rest 
  | [] -> code, lastChanged 

let rec run_all ?(iter=1) fnTable maxiters optimizations code =      
  let code', changed = fold_optimizations fnTable code false optimizations in 
  if changed && iter < maxiters then 
    run_all ~iter:(iter+1) fnTable maxiters optimizations code'  
  else code', iter
  

let optimize_block 
      ?(maxiters=100) 
      (fnTable : FnTable.t)  
      (block  : SSA.block)  
      optimizations = run_all fnTable maxiters optimizations block 
       
let optimize_fundef 
      ?(maxiters=100) 
      (fnTable : FnTable.t) 
      (fundef : SSA.fundef) 
      (optimizations : (string * optimization) list)
       = run_all fnTable maxiters optimizations fundef
      
(* update each function in the unoptimized queue of the FnTable *)  
let optimize_all_fundefs ?(maxiters=100) (fnTable : FnTable.t) optimizations =
  while FnTable.have_unoptimized fnTable do
    let fundef = FnTable.get_unoptimized fnTable in 
    let optimized, iters = 
      optimize_fundef ~maxiters fnTable fundef optimizations 
    in
    IFDEF DEBUG THEN 
      assert (fundef.fn_id = optimized.fn_id);
      Printf.printf "Optimizing %s..."; 
      let status = 
        if iters > 1 then  
          Printf.sprintf "modified (%d iters):\n %s" 
            iters 
            (SSA.fundef_to_str optimized)
         else ""  
      in 
      Printf.printf "%s\n" (FnId.to_str fundef.fn_id) status;
    ENDIF;
    FnTable.update  optimized fnTable       
  done  