(* pp: -parser o pa_macro.cmo *)

open Base
open SSA

let rec process_block mapping typedClosureEnv  stmts =  stmts 
(*function 
  | [] -> []
  | stmt::rest -> 
      let stmts = (process_stmt mapping typedClosureEnv stmt) in 
      stmts @ process_block  mapping typedClosureEnv rest

and process_stmt 
      (mapping: ID.Set.t ID.Map.t)
      (typedClosureEnv : 
        (SSA.value_node * DynType.t list * DynType.t list) ID.Map.t)
      stmtNode = 
  match stmtNode.stmt with
  (* we're assumign closures don't get reassigned via 
     Values nodes *)
  | Set ([id], {exp=App(_, args)}) 
     when ID.Map.mem id mapping ->
    
    let argTypes = List.map (fun v -> v.value_type) args in
    let typedClosureSet = ID.Map.find id mapping in 
    let typedClosureList = ID.Set.elements typedClosureSet in
    
    List.map 
      (fun typedId ->
        IFDEF DEBUG THEN 
        Printf.printf "[InsertTypedClosures] Rewriting _x%d to _x%d\n;"
          id typedId
        ;  
        ENDIF; 
        let (typedFnVal, futureArgTypes, retTypes) = 
          ID.Map.find typedId typedClosureEnv 
        in
        let fnT = typedFnVal.value_type in
        let closureType = DynType.FnT(argTypes, futureArgTypes, retTypes) in   
        SSA.mk_set [typedId] 
          (SSA.mk_app ~types:[closureType] typedFnVal  args)
      )
      typedClosureList
  | Set ([id], exp) when List.for_all ((=) DynType.BottomT) exp.exp_types ->
     IFDEF DEBUG THEN 
       Printf.printf 
         "[InsertTypedClosures] Removing useless statement: %s = %s\n"
         (ID.to_str id)
         (SSA.exp_to_str exp)
       ;
    ENDIF; 
    []
  
  | _ ->  [stmtNode] 
*)  