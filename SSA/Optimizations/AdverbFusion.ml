(* pp: -parser o pa_macro.cmo *)

open Base
open Type
open Adverb
open TypedSSA
open AdverbHelpers
open SSA_Transform

(* resolve the dependency between fnmanager and this module *)

module Fusion_Rules = struct
  	let mk_global_compose_fn fid1 fid2 =
	  (* get two functions from FnTable *) 
	  let fn1 = FnManager.get_typed_function fid1 in
	  let fn2 = FnManager.get_typed_function fid2 in
	  (* argument and return var nodes *)
	  let args      = List.map (fun ty -> var ty (ID.gen ()) ) fn1.fn_input_types in
	  let ret_args  = List.map (fun ty -> var ty (ID.gen ()) ) fn1.fn_output_types in
	  let ret_args2 = List.map (fun ty -> var ty (ID.gen ()) ) fn2.fn_output_types in
	  (* call expressions *)
	  let call_expr1 = call fid1 fn1.fn_output_types args in
	  let call_expr2 = call fid2 fn2.fn_output_types ret_args in 
	  (* argument ids, return ids *)
	  let args_ids  = get_ids args in
	  let ret_ids   = get_ids ret_args in
	  let ret_ids2  = get_ids ret_args2 in
	  (* statements *)
	  let stmt1 = set args_ids call_expr1 in
	  let stmt2 = set ret_ids call_expr2 in 
	  (* create a function's body *)
	  let body = Block.singleton stmt1 in 
	  let _ = Block.add body stmt2 in
	  (* create a new function *)
	  let new_fn = TypedSSA.mk_fn ?name:None ~tenv:fn1.tenv ~input_ids:args_ids ~output_ids:ret_ids2 ~body:body in
	     (* register the new global function to FnTable and return it *)
	  let _ = FnManager.add_typed new_fn in 
	    new_fn
	
  (* context always has a previous immediate adverb function *)
  type context = (stmt_node option) ref 
  let preImmStmt = ref None
  let init _ = ref None
  let finalize _ _ = NoChange
  let dir = Forward

  let stmt env stmtNode = match stmtNode.stmt with
    (* handlig only an single assignment *)
    | Set([id],{exp=Adverb{adverb=Map;adverb_fn=adverb_fn1;fixed_args=fixed_args1;axes=axes1;array_args=array_args1}}) -> begin match !preImmStmt with
      | None -> let _ =  preImmStmt := Some stmtNode in Update empty_stmt
      | Some s_node -> begin match s_node.stmt with
        | Set([id2],{exp=Adverb{adverb=Map;adverb_fn=adverb_fn2;fixed_args=fixed_args2}}) ->(* check the rule, map ( g, map(f,array) ) = map (compose(f,g), array) *)			      					
          if adverb_fn1 = id then 
	          let new_fn = mk_global_compose_fn adverb_fn1 adverb_fn2 in
	          let adverb_info = 
                { adverb = Map; 
                  adverb_fn = new_fn.fn_id;
	              fixed_args = List.append fixed_args1 fixed_args2;
               	  init = None;
                  axes = axes1;
                  array_args = array_args1;
                } in 
             let new_adverb_fn = mk_adverb_exp_node adverb_info in 
             let new_set = set [id2] new_adverb_fn in 	           
                Update new_set
            else UpdateWithStmts(stmtNode,[s_node]) 
   	    | _ -> UpdateWithStmts(stmtNode,[s_node]) end (* End of Some s_node *)
      end (* End of Set([id],{exp=Adverb...}) *) 
    | _ -> begin match !preImmStmt with 
      | None -> NoChange 
	  | Some s_node -> UpdateWithStmts (stmtNode,[s_node]) end (* End of !preImmStmt *)
	
		
  let phi   env phiNode = NoChange
  let exp   env expNode = NoChange
  let value env valNode = NoChange 


end

module Fusion_Rewrite = SSA_Transform.Mk(Fusion_Rules)

let optimize_fn _ fn = Fusion_Rewrite.transform_fn fn
