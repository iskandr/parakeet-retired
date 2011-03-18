(* pp: -parser o pa_macro.cmo *)

open Base
open DynType 
open SSA
open SSA_Transform
open FindUseCounts
open FindDefs
 
module SimplifyRules = struct
  let dir = Forward
   
  type context = {
    constants: SSA.value ConstantLattice.t ID.Map.t;
    defs : (ID.t, DefLattice.t) Hashtbl.t;  
    use_counts : (ID.t, int) Hashtbl.t; 
    types : DynType.t ID.Map.t; 
  } 
      
  let init fundef = 
    {
      constants = FindConstants.find_constants fundef;
      defs = FindDefs.find_defs fundef;  
      use_counts = FindUseCounts.find_fundef_use_counts fundef;
      types = fundef.tenv;   
    }
    
  (* since all outputs are considered used, dummy assignments leading to an *)
  (* output don't get cleaned up. This final step gets rid of stray assignments*)
  (* at the end of a function *)  
  let finalize cxt fundef =
    let inputIdSet = ID.Set.of_list fundef.input_ids in 
    let outputIds = 
      List.map 
      (fun id -> match Hashtbl.find cxt.defs id with 
        | FindDefs.DefLattice.Val (Var prevId) -> 
            (* an ID can't be both input and output *) 
            if ID.Set.mem prevId inputIdSet then id else prevId   
        | _ -> id)
      fundef.output_ids 
    in 
    if List.eq_elts outputIds fundef.output_ids then NoChange
    else Update {fundef with output_ids = outputIds } 
    
  let is_live cxt id = 
    Hashtbl.mem cxt.use_counts id && Hashtbl.find cxt.use_counts id > 0   
  
  
  let stmt cxt stmtNode = match stmtNode.stmt with 
    | Set (ids, ({exp=Values vs} as expNode)) -> 
      let pairs = List.combine ids vs in 
      let  livePairs, deadPairs = 
        List.partition (fun (id,_) -> is_live cxt id) pairs 
      in
      if deadPairs = [] then NoChange
      else if livePairs = [] then Update SSA.empty_stmt 
      else 
        let liveIds, liveValues = List.split livePairs in
        let rhs = {expNode with exp=Values liveValues} in  
        Update (SSA.mk_set ?src:stmtNode.stmt_src liveIds rhs)      
    | Set (ids, exp) ->
        let rec any_live = function 
          | [] -> false 
          | id::rest -> (is_live cxt id) || any_live rest
        in 
        if any_live ids then NoChange 
        else Update SSA.empty_stmt
           
    | If (condVal, tBlock, fBlock, merge) ->
      let get_type id = ID.Map.find id cxt.types in
      begin match condVal.value with 
        | Num (PQNum.Bool b) ->
            let ids, valNodes = SSA.collect_phi_values b merge in 
            let types = List.map get_type ids in 
            let expNode = 
              SSA.mk_exp ?src:stmtNode.stmt_src ~types (SSA.Values valNodes) 
            in 
            Update (SSA.mk_set ?src:stmtNode.stmt_src ids expNode)
        | _ -> NoChange  
      end
    | SSA.WhileLoop (testBlock, testVal, body, header, exit) -> NoChange  
      
    (* 
      begin match test.test_value.value with 
        | Num PQNum.Bool false -> 
            (* leave the condBlock for potential side effects, *)
            (* get rid of loop *) 
            UpdateWithBlock (SSA.empty_stmt, test.test_block)
        | _ -> NoChange
      end*)     
    | _ -> NoChange 
  
  let exp cxt expNode = NoChange 
  
  let phi cxt phiNode = NoChange 
  
  let value cxt valNode = match valNode.value with
    | Var id -> 
      begin match ID.Map.find_option id cxt.constants with 
        | Some ConstantLattice.Const v -> Update {valNode with value = v }
        | Some _ 
        | None -> 
          (match Hashtbl.find cxt.defs id with 
            | FindDefs.DefLattice.Val v -> Update {valNode with value = v }
            | _ -> NoChange
          )   
      end
    | _ -> NoChange  
end

module Simplifer = SSA_Transform.Mk(SimplifyRules)

let simplify_fundef (_ : FnTable.t) = Simplifer.transform_fundef 
  
