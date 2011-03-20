(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Transform

type closure_env = { 
  closures : (ID.t, value) Hashtbl.t; 
  closure_args : (ID.t, value_node list) Hashtbl.t;
  closure_arity : (ID.t, int) Hashtbl.t; 
}
 
module CollectRules(F:sig val interpState: InterpState.t end) = struct 
  type context = closure_env 

  let init _ = { 
    closures = Hashtbl.create 127; 
    closure_args = Hashtbl.create 127;
    closure_arity = Hashtbl.create 127; 
  }  
  
  let finalize _ _ = NoChange 
  
  let dir = Forward 
  
  let rec min_arity env = function 
  | Prim op -> Prim.min_prim_arity op
  | GlobalFn fnId -> InterpState.get_untyped_arity F.interpState fnId
  | Var closureId ->
      if Hashtbl.mem env.closure_arity closureId then 
        Hashtbl.find env.closure_arity closureId
      else 0 (* if variable isn't already a closure, assume it's an array *) 
  | other -> failwith $ 
     Printf.sprintf 
       "Can't get arity of %s" 
       (SSA.value_to_str other)
      
  let max_arity context = function 
  | Prim op -> Prim.max_prim_arity op 
  | other -> min_arity context other 

  let stmt env stmtNode =  match stmtNode.stmt with 
    | Set([closureId], ({exp=App(f, args)} as expNode)) -> 
        let minArgs = min_arity env f.value in 
        let numArgs = List.length args in 
        if numArgs >= minArgs then NoChange
        else 
          (* - create fresh IDs for all the arguments*)
          (* - assign the arguments to their fresh IDs*)
          (* - record the new IDs in env.closure_args*)
          (* - replace the partial application with the closure arg assignment*)  
          let closureArgIds = ID.gen_fresh_list numArgs in
          let closureArgNodes = 
            List.map (fun id -> mk_var ?src:expNode.exp_src id) closureArgIds
          in  
          Hashtbl.add env.closures closureId f.value;
          Hashtbl.add env.closure_args closureId closureArgNodes; 
          Hashtbl.add env.closure_arity closureId (minArgs - numArgs);
          let argsExp = SSA.mk_exp ?src:stmtNode.stmt_src (Values args) in
          Update (SSA.mk_set ?src:stmtNode.stmt_src closureArgIds argsExp) 
    | _ -> NoChange

  let phi env phiNode = NoChange                            
  let exp _ _ = NoChange   
  let value _ _  = NoChange    
end 

let collect_partial_apps interpState fundef = 
 let module Collector = 
  Mk(CollectRules(struct let interpState = interpState end))
 in
(*
 IFDEF DEBUG THEN 
   Printf.printf "Collect Partial Apps (before): %s\n%!"
     (SSA.fundef_to_str fundef);
 ENDIF;
*)
 let fundef', _ = Collector.transform_fundef fundef in
 let closureEnv = Collector.get_context () in
(*
 IFDEF DEBUG THEN 
   Printf.printf "Collect Partial Apps (after): %s\n%!"
     (SSA.fundef_to_str fundef');
 ENDIF;
*)
 fundef', closureEnv 
