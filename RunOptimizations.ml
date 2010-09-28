open Base 

let rec fold_optimizations fnTable code lastChanged = function
  | (name, opt)::rest -> 
      debug (Printf.sprintf "[optimizer] Running %s: " name);  
      let code', changed = opt fnTable code in 
      debug (Printf.sprintf "...%s\n" (if changed then "MODIFIED" else "done"));  
      fold_optimizations fnTable code' (changed || lastChanged)  rest 
  | [] -> code, lastChanged 

let rec run_all ?(iter=1) fnTable maxIters code optimizations =      
  let code', changed = fold_optimizations fnTable code false optimizations in 
  if changed && iter < maxIters then 
    run_all ~iter:(iter+1) fnTable maxIters code' optimizations 
  else code' 
  

let optimize_block 
      ?(maxIters=100) 
      ?(inline=false) 
      (fnTable : FnTable.t) 
      (block  : SSA.block) 
      optimizations = 
  let block' = run_all fnTable maxIters block optimizations in
  if inline then
    begin 
      debug "[optimizer] Inlining functions...\n";
      let inlinedCode, inlineChanged = UntypedInline.run_block_inliner block' in 
      if inlineChanged then run_all fnTable maxIters inlinedCode optimizations
      else block'
    end  
  else block' 

let optimize_fundef 
      ?(maxIters=100) 
      ?(inline=false) 
      (fnTable : FnTable.t) 
      (fundef : SSA.fundef) 
      optimizations  =
  let fundef' = run_all fnTable maxIters fundef optimizations in
  if inline then
    begin 
      debug "[optimizer] Inlining functions...\n";
      let inlinedCode, inlineChanged = 
        UntypedInline.run_fundef_inliner fnTable fundef' in 
      if inlineChanged then run_all fnTable maxIters inlinedCode optimizations
      else fundef'
    end  
  else fundef' 
 