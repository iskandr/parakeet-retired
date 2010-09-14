
let rec fold_optimizations block lastChanged = function
  | (name, opt)::rest -> 
      Printf.printf "[optimizer] Running %s: " name; 
      let block', changed = opt block in 
      Printf.printf "%s\n" (if changed then "MODIFIED" else "");  
      fold_optimizations block' (changed || lastChanged)  rest 
  | [] -> block, lastChanged 

let rec run_all ?(iter=1) maxIters block optimizations =      
  let block', changed = fold_optimizations block false optimizations in 
  if changed && iter < maxIters then 
    run_all ~iter:(iter+1) maxIters block' optimizations 
  else block' 
  

let optimize_block ?(maxIters=100) ?inliner block optimizations = 
  let block' = run_all maxIters block optimizations in 
  match inliner with 
    | None -> block' 
    | Some inliner ->  
      Printf.printf "[optimizer] Inlining functions...\n";
      let inlinedCode, inlineChanged = inliner block' in 
      if inlineChanged then run_all maxIters inlinedCode optimizations
      else block'  
