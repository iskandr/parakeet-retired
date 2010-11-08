open Base 


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
      (*debug "[optimizer] Inlining functions...\n";*)
      let inlinedCode, inlineChanged = Inline.run_block_inliner block' in 
      if inlineChanged then aux inlinedCode 
      else block'
    end  
  else block' 

let rec exhaustive_inline ?(iter=0) fnTable optimizer fundef =
  if iter > 10 then fundef 
  else 
    let inlinedCode, inlineChanged = Inline.run_fundef_inliner fnTable fundef in
    if inlineChanged then
      let optimized = optimizer inlinedCode in
      exhaustive_inline ~iter:(iter+1) fnTable optimizer optimized
    else inlinedCode
    

let optimize_fundef 
      ?(maxiters=100) 
      ?(inline=false) 
      (fnTable : FnTable.t) 
      (fundef : SSA.fundef) 
      (optimizations : (string * optimization) list) =
 
    let optimizer : SSA.fundef -> SSA.fundef = 
      run_all fnTable maxiters optimizations
    in 
    let optimized = optimizer fundef  in
    if inline then exhaustive_inline fnTable optimizer optimized
    else optimized
    