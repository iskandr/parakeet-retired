(* pp: -parser o pa_macro.cmo *)

open Base 
open SSA

type optimization = FnTable.t -> SSA.fundef -> SSA.fundef * bool   

let rec fold_optimizations ?(type_check=false) fnTable fundef lastChanged = 
  function
  | (name, opt)::rest -> 
      (*Timing.start_timer ("opt::"^name);*)
      IFDEF DEBUG THEN Printf.printf "Running %s...\n%! " name; ENDIF; 
      let optimized, changed = opt fnTable fundef in
      IFDEF DEBUG THEN

        if changed then 
          Printf.printf "Changes caused by %s:\n Old: %s\n New: %s\n%!"
            name
            (SSA.fundef_to_str fundef)
            (SSA.fundef_to_str optimized)
        ; 
        if type_check then  
          let errorLog = TypeCheck.check_fundef optimized in 
          if not $ Queue.is_empty errorLog then ( 
          Printf.printf 
            "--- Errors found in %s after %s ---\n%!"
            (FnId.to_str fundef.SSA.fn_id)
            name
          ;
          TypeCheck.print_all_errors errorLog; exit 1 
       
        );
      ENDIF;
      (*Timing.stop_timer ("opt::"^name);*)
      fold_optimizations fnTable optimized (changed || lastChanged)  rest 
  | [] -> fundef, lastChanged
   
let rec optimize_fundef
      ?(type_check=true)
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
      Printf.printf "[RunOptimizations] Starting to optimize: %s\n"
        (SSA.fundef_to_str fundef)
      ;   
      if type_check then  (
        let errorLog = TypeCheck.check_fundef fundef in 
        if not $ Queue.is_empty errorLog then (
          Printf.printf 
            "--- found errors in %s before optimization ---\n%s\n" 
            (FnId.to_str fundef.SSA.fn_id)
            (SSA.fundef_to_str fundef)
          ;
          TypeCheck.print_all_errors errorLog;
          exit 1  
        )
      );
    ENDIF; 
    let optimized, iters = 
      optimize_fundef ~type_check ~maxiters fnTable fundef optimizations 
    in
    IFDEF DEBUG THEN 
      assert (fundef.fn_id = optimized.fn_id);
      if iters > 1 then  
        Printf.printf "--- %s modified after %d optimization iters:\n%s\n\n%!"
            (FnId.to_str fundef.fn_id) 
            iters 
            (SSA.fundef_to_str optimized)
      ;
    ENDIF;
    FnTable.update optimized fnTable       
  done
  