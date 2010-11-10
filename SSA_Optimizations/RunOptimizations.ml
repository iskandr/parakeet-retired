open Base 
open SSA

type optimization = FnTable.t -> SSA.fundef -> SSA.fundef * bool   

let rec fold_optimizations ?(type_check=false) fnTable fundef lastChanged = 
  function
  | (name, opt)::rest -> 
      let fundef', changed = opt fnTable fundef in
      IFDEF DEBUG THEN  
        if type_check then  
        let errorLog = TypeCheck.check_fundef fundef' in 
        if not $ Queue.is_empty errorLog then ( 
        Printf.printf 
            "--- Errors found in %s after %s ---\n"
            (FnId.to_str fundef.SSA.fn_id)
            name
          ;
          Printf.printf "[BEFORE OPTIMIZATION] \n%s\n" 
            (SSA.fundef_to_str fundef)
          ;
          Printf.printf "[AFTER OPTIMIZATION] \n%s\n"
            (SSA.fundef_to_str fundef)
          ;
          TypeCheck.print_all_errors errorLog; exit 1 
       
        );
      ENDIF;
      fold_optimizations fnTable fundef' (changed || lastChanged)  rest 
  | [] -> fundef, lastChanged
(*
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
*)       
let rec optimize_fundef
      ?(type_check=false)
      ?(iter=1) 
      ?(maxiters=100) 
      (fnTable : FnTable.t) 
      (fundef : SSA.fundef) 
      (optimizations : (string * optimization) list) =
  let fundef', changed = 
    fold_optimizations ~type_check fnTable fundef false optimizations 
  in
  if changed && iter < maxiters then 
    optimize_fundef 
      ~type_check 
      ~iter:(iter+1)  
      ~maxiters 
      fnTable 
      fundef' 
      optimizations   
  else fundef', iter
      
(* update each function in the unoptimized queue of the FnTable *)  
let optimize_all_fundefs 
      ?(type_check=false) 
      ?(maxiters=100) 
      (fnTable : FnTable.t) 
      (optimizations : (string * optimization) list) =
  while FnTable.have_unoptimized fnTable do
    let fundef = FnTable.get_unoptimized fnTable in
    IFDEF DEBUG THEN  
        if type_check then  
        let errorLog = TypeCheck.check_fundef fundef in 
        if not $ Queue.is_empty errorLog then (
          Printf.printf "--- found errors in %s before optimization ---\n%s\n" 
            (FnId.to_str fundef.SSA.fn_id)
            (SSA.fundef_to_str fundef);
          TypeCheck.print_all_errors errorLog;
          exit 1  
        )
    ENDIF; 
    let optimized, iters = 
      optimize_fundef ~type_check ~maxiters fnTable fundef optimizations 
    in
    IFDEF DEBUG THEN 
      assert (fundef.fn_id = optimized.fn_id);
      Printf.printf "Optimizing %s..." (FnId.to_str fundef.fn_id); 
      let status = 
        if iters > 1 then  
          Printf.sprintf "modified (%d iters):\n %s" 
            iters 
            (SSA.fundef_to_str optimized)
         else ""  
      in 
      Printf.printf "%s\n" status;
    ENDIF;
    FnTable.update  optimized fnTable       
  done  