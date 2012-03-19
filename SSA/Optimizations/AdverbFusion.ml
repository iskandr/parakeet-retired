(* pp: -parser o pa_macro.cmo *)

open Base
open Type
open Adverb
open TypedSSA
open SSA_Transform

(* resolve the cyclic dependency between fnmanager and this module *)
let get_typed_fn = ref (fun fid -> (failwith "get_typed not initialized": TypedSSA.fn )) 
let add_typed_fn = ref (fun ?optimize:bool fid -> (failwith "add_typed not initialized":unit)) 

let init_get_typed_fn fn = get_typed_fn := fn
let init_add_typed_fn fn = add_typed_fn := fn

module Fusion_Rules = struct
  let mk_global_compose_fn fid1 fid2 =
  (* get two functions from FnTable *) 
    let fn1 = !get_typed_fn fid1 in
    let fn2 = !get_typed_fn fid2 in
  (* argument and return var nodes *)
    let args   = List.map (fun ty -> var ty (ID.gen ()) ) fn1.fn_input_types in
    let locals = List.map (fun ty -> var ty (ID.gen ()) ) fn1.fn_output_types in
    let rets   = List.map (fun ty -> var ty (ID.gen ()) ) fn2.fn_output_types in
    (* call expressions *)
    let call_expr1 = call fid1 fn1.fn_output_types args in
    let call_expr2 = call fid2 fn2.fn_output_types locals in 
    (* argument ids, return ids *)
    let arg_ids   = get_ids args in
    let local_ids = get_ids locals in
    let ret_ids   = get_ids rets in
    (* statements *)
    let stmt1 = set local_ids call_expr1 in
    let stmt2 = set ret_ids call_expr2 in 
    (* create a function's body *)
    let body = Block.singleton stmt1 in 
    let _ = Block.add body stmt2 in
    (* create a new function *)
    let tenv' = ID.Map.of_lists (arg_ids @ ret_ids @ local_ids) (fn1.fn_input_types @ fn2.fn_output_types @ fn1.fn_output_types) in
    let new_fn = TypedSSA.mk_fn ?name:None ~tenv:tenv' ~input_ids:arg_ids ~output_ids:ret_ids ~body:body in
    (* register the new global function to FnTable and return it *)
    let _ = !add_typed_fn new_fn in 
      new_fn

(* redefined adverb_out_type in AdverbHelpers because of cyclic dependancy *)	
  let adverb_return_types adverb numAxes nestedOutTypes =
    match adverb with 
    | Scan 
    | Map  ->
      List.map (Type.increase_rank numAxes) nestedOutTypes 
    | AllPairs ->
      List.map (Type.increase_rank (numAxes * 2)) nestedOutTypes 
    | Reduce -> nestedOutTypes

  type context = (stmt_node option) 
  (* context always has a previous immediate adverb function *)
  let preImmStmt = ref None
  let init _ = None
  let finalize _ _ = NoChange
  let dir = Forward

  let stmt env stmtNode = match stmtNode.stmt with
    (* handlig only an single assignment *)
    (* Map with length(array_args) = 1 *)
    | Set(ids1,{exp=Adverb{adverb=Map;adverb_fn=adverb_fn1;fixed_args=fixed_args1;init=_;axes=_; array_args=[array_arg1]}}) -> begin match !preImmStmt with
      | None -> let _ =  preImmStmt := Some stmtNode in IFDEF DEBUG THEN Printf.printf "SKIPP...\n%! " ; ENDIF; ReplaceWithStmts (empty_stmt,[])
      | Some s_node -> begin match s_node.stmt with
        | Set([id2],{exp=Adverb{adverb=Map;adverb_fn=adverb_fn2;fixed_args=fixed_args2;axes=axes2;array_args=array_args2}}) ->
          if (value_to_str array_arg1.value) = (ID.to_str id2) then (* Apply the Map Rule *)
              let _ = Printf.printf "MATCH!!...\n" in 
	          let new_fn = mk_global_compose_fn adverb_fn2 adverb_fn1 in
	          let adverb_info = 
                { adverb = Map; 
                  adverb_fn = new_fn.fn_id;
	              fixed_args = List.append fixed_args2 fixed_args1;
               	  init = None;
                  axes = axes2;
                  array_args = array_args2;
                } in 
              let new_adverb_fn = 
               { 
                 exp = Adverb adverb_info;
                 exp_types = adverb_return_types adverb_info.adverb (List.length axes2) (FnHelpers.output_types new_fn);
                 exp_src = None;
               } in 
              let new_set = set ids1 new_adverb_fn in 	           
                preImmStmt := None;Update new_set
            else ( Printf.printf "NOT MATACH1..%s = %s\n" (value_to_str array_arg1.value) (ID.to_str id2); preImmStmt := None; UpdateWithStmts(stmtNode,[s_node]))
   	    | _ -> ( IFDEF DEBUG THEN Printf.printf "NOT MATACH2...\n" ; ENDIF; preImmStmt := None ; ReplaceWithStmts(stmtNode,[s_node]) ) end (* End of Some s_node *)
      end (* End of Set([id],{exp=Adverb...}) *) 
    (* other Maps matching *)
    | Set(ids1,{exp=Adverb{adverb=Map;adverb_fn=adverb_fn1;fixed_args=fixed_args1;init=_;axes=_; array_args=array_args1}}) -> begin match !preImmStmt with
      | None ->  let _ =  preImmStmt := Some stmtNode in IFDEF DEBUG THEN Printf.printf "SKIPP2...\n%! " ; ENDIF; ReplaceWithStmts (empty_stmt,[])
      | Some s_node -> (preImmStmt := None; UpdateWithStmts (stmtNode,[s_node])) end
    | _ -> begin match !preImmStmt with 
      | None -> (IFDEF DEBUG THEN Printf.printf "NOT MATACH3...\n" ; ENDIF; NoChange) 
	  | Some s_node -> IFDEF DEBUG THEN Printf.printf "UPdate stmtNodUpdate ...\n%! "; ENDIF; preImmStmt := None; ReplaceWithStmts (stmtNode,[s_node]) end (* End of !preImmStmt *)
			
  let phi   env phiNode = NoChange
  let exp   env expNode = NoChange
  let value env valNode = NoChange 

end

module Fusion_Rewrite = SSA_Transform.Mk(Fusion_Rules)

let optimize_fn _ fn = Fusion_Rewrite.transform_fn fn
