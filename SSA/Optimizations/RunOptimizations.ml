(* pp: -parser o pa_macro.cmo *)

open Base 
open SSA

type optimization = FnTable.t -> SSA.fn -> SSA.fn * bool   

let rec fold_optimizations ?(type_check=false) fnTable fn lastChanged = 
  function
  | (name, opt)::rest -> 
      (*Timing.start_timer ("opt::"^name);*)
      (*
      IFDEF DEBUG THEN Printf.printf "Running %s...\n%! " name; ENDIF;
      *) 
      let optimized, changed = opt fnTable fn in
      IFDEF DEBUG THEN
        if type_check then  
          let errorLog = TypeCheck.check_fn optimized in 
          if not $ Queue.is_empty errorLog then ( 
          Printf.printf 
            "--- Errors found in %s after %s ---\n%!"
            (FnId.to_str fn.SSA.fn_id)
            name
          ;
          TypeCheck.print_all_errors errorLog; exit 1 
       
        );
      ENDIF;
      (*Timing.stop_timer ("opt::"^name);*)
      fold_optimizations fnTable optimized (changed || lastChanged)  rest 
  | [] -> fn, lastChanged
   
let rec optimize_fn
      ?(type_check=true)
      ?(iter=1) 
      ?(maxiters=100) 
      (fnTable : FnTable.t) 
      (fn : SSA.fn) 
      (optimizations : (string * optimization) list) =
  let fn', changed = 
    fold_optimizations ~type_check fnTable fn false optimizations 
  in
  if changed && iter < maxiters then 
    optimize_fn 
      ~type_check 
      ~iter:(iter+1)  
      ~maxiters 
      fnTable 
      fn' 
      optimizations   
  else fn', iter
      
(* update each function in the unoptimized queue of the FnTable *)  
let optimize_all_fns 
      ?(type_check=false) 
      ?(maxiters=100) 
      (fnTable : FnTable.t) 
      (optimizations : (string * optimization) list) =
  while FnTable.have_unoptimized fnTable do
    let fn = FnTable.get_unoptimized fnTable in
    IFDEF DEBUG THEN
      (*
      Printf.printf "[RunOptimizations] Starting to optimize: %s\n"
        (SSA.fn_to_str fn)
      ;  
      *) 
      if type_check then  (
        let errorLog = TypeCheck.check_fn fn in 
        if not $ Queue.is_empty errorLog then (
          Printf.printf 
            "--- found errors in %s before optimization ---\n%s\n" 
            (FnId.to_str fn.SSA.fn_id)
            (SSA.fn_to_str fn)
          ;
          TypeCheck.print_all_errors errorLog;
          exit 1  
        )
      );
    ENDIF; 
    let optimized, iters = 
      optimize_fn ~type_check ~maxiters fnTable fn optimizations 
    in
    IFDEF DEBUG THEN 
      assert (fn.fn_id = optimized.fn_id);
      if iters > 1 then  
        Printf.printf "--- %s modified after %d optimization iters:\n%s\n\n%!"
            (FnId.to_str fn.fn_id) 
            iters 
            (SSA.fn_to_str optimized)
      ;
    ENDIF;
    FnTable.update optimized fnTable       
  done
  