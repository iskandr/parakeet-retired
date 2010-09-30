open Base
open Imp

(* actually a semi-lattice due to the absence of a 'meet' function *) 
module type LATTICE = sig
  type t 
  val top : t
  val bottom : t 
  val join : t -> t -> t
end

module type SEMANTICS =   sig
   module Lattice : LATTICE
   val eval_exp :(ID.t, Lattice.t) PMap.t -> Imp.exp -> Lattice.t  
end


(* flow insensitive dataflow analysis. *) 

module Make(S : SEMANTICS) = struct 
  let rec eval_block env code = 
    List.fold_left block_folder (env, false) code   
  and block_folder (accEnv, accChanged) stmt = 
    let accEnv', changed = eval_stmt accEnv stmt in 
    accEnv', changed || accChanged 
     
  (* if the value lattice is of finite height, this iteration should converge
      but put a maxiters of 100 just in case 
  *) 
  and iterate ?(niters = 1) env block = 
    let env', changed = eval_block env block in 
    if changed || niters > 100 then iterate ~niters:(niters + 1) env' block 
    else env, niters > 1 
  and eval_stmt env stmt =
    debug (Printf.sprintf "[imp_abstract_interp] eval_stmt: %s%!"
           (Imp.stmt_to_str stmt)); 
          
    match stmt with
    | If (_, tBlock, fBlock) -> 
      let env1, changed1 = eval_block env tBlock in 
      let env2, changed2 = eval_block env1 fBlock in 
      env2, changed1 || changed2  
    | While (_, code) -> iterate env code 
    | Set (id, rhs) ->  
      let newVal = S.eval_exp env rhs in
      begin 
        let oldVal = PMap.find_default id env S.Lattice.bottom in
        let joinedVal = S.Lattice.join newVal oldVal in 
        if joinedVal <> oldVal then (PMap.add id joinedVal env, true)
        else (env, false)
      end   
    | _ (* comments or syncthreads *) -> env, false  

  let run env code = 
    debug (Printf.sprintf "started running imp analysis %!");
    let env', _ = List.fold_left block_folder (env, false) code in 
    debug (Printf.sprintf "done running imp analysis %!");
    
    env' 
  
      
end
