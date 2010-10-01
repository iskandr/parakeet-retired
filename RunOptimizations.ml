open Base 

let rec fold_optimizations fnTable code lastChanged = function
  | (name, opt)::rest -> 
      debug (Printf.sprintf "[optimizer] Running %s: " name);  
      let code', changed = opt fnTable code in 
      debug (Printf.sprintf "...%s\n" (if changed then "MODIFIED" else "done"));  
      fold_optimizations fnTable code' (changed || lastChanged)  rest 
  | [] -> code, lastChanged 

let rec run_all ?(iter=1) fnTable maxiters optimizations code =      
  let code', changed = fold_optimizations fnTable code false optimizations in 
  if changed && iter < maxiters then 
    run_all ~iter:(iter+1) fnTable maxiters optimizations code'  
  else code' 
  

let optimize_block 
      ?(maxiters=100) 
      ?(inline=false) 
      (fnTable : FnTable.t) 
      (block  : SSA.block) 
      optimizations =
  let aux : SSA.block -> SSA.block  = run_all fnTable maxiters optimizations in   
  let block' = aux block in
  if inline then
    begin 
      debug "[optimizer] Inlining functions...\n";
      let inlinedCode, inlineChanged = Inline.run_block_inliner block' in 
      if inlineChanged then aux inlinedCode 
      else block'
    end  
  else block' 

let optimize_fundef 
      ?(maxiters=100) 
      ?(inline=false) 
      (fnTable : FnTable.t) 
      (fundef : SSA.fundef) 
      optimizations  =
  debug $ Printf.sprintf "[optimize_fundef] running optimzer on function: %s" 
    (SSA.fundef_to_str fundef);         
  let aux : SSA.fundef -> SSA.fundef  = 
    run_all fnTable maxiters optimizations 
  in  
  let fundef' = aux fundef  in
  let fundef'' = 
    if inline then
    begin 
      debug "[optimizer] Inlining functions...\n";
      let inlinedCode, inlineChanged = 
        Inline.run_fundef_inliner fnTable fundef' in 
      if inlineChanged then (
        debug $ Printf.sprintf 
          "[optimize_fundef] inlined function: %s "
          (SSA.fundef_to_str inlinedCode); 
        aux inlinedCode
      )
      else fundef'
    end
    else fundef'  
  in 
  debug $ Printf.sprintf "[optimize_fundef] optimized function: %s" 
    (SSA.fundef_to_str fundef);         
  fundef''    
 