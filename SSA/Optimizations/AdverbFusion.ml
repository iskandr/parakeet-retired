
open TypedSSA
open Adverb 
open SSA_Analysis
open SSA_Transform 
(* 

1) Gather data flow information, collecting which statements 
   produced which variables 
2) Look for linear adverb flow i.e.:
    S1: y = map(f, x); 
    S2: z = map(g, y)
   These can be fused as DELETE(S1), REPLACE(S2, map(f.g, x))
   if 
      consumes(S2) = produces(S1)
      axes(S1) = axes(S2) 
      |consumed_by(x)| = 1
      
3) Process instructions by scanning through statements and deleting statements    
*)

module DataFlow = struct 
  type env = { 
    produces : ID.t list StmtId.Map.t; 
    produced_by : StmtId.t ID.Map.t;
    consumes: ID.Set.t StmtId.Map.t;
    consumed_by : StmtId.Set.t ID.t list; 
    adverbs :  (FnId.t, value_nodes, value_nodes) Adverb.info StmtId.Map.t; 
  }
  type value_info = unit 
  type exp_info = unit 
  
  let dir = SSA_Analysis.Forward 
  let iterative = false 
  let init _ = 
    { 
      produces = StmtId.Map.empty; 
      produced_by = ID.Map.empty; 
      consumes = StmtId.Map.empty; 
      consumed_by = ID.Map.empty; 
      adverbs = StmtId.Map.empty; 
    }
  
  let stmt env stmtNode stmtHelpers = None 
  
  let value _ _ = ()
  let exp _ _ _ = ()
  let phi_set _ _ _ = None
  let phi_merge _ _ _ _ = None
end 

module DataFlowEval = SSA_Analysis.MkEvaluator(DataFlow)
let gather_data_flow_info fn = DataFlowEval.eval_fn fn 


let fresh_like id = ID.gen_named (ID.get_original_prefix id)
let rec split_nth n xs =
  if n <= 0 then [], xs 
  else match xs with 
    | [] -> failwith "[split_nth] List too short"
    | x::xs -> 
      let ys, zs = split_nth (n-1) xs in
      x::ys, zs
  
   
let fuse_adverbs pred succ =
  assert (pred.init = None);
  assert (pred.adverb = Adverb.Map);
  let predFn = FnManager.get_typed_function pred.adverb_fn in 
  let succFn = FnManager.get_typed_function succ.adverb_fn in 
  (* 
     Make a new function:
        lambda(pred_fixed @ succ_fixed @ pred_elts): 
          succ_elts = Call(predFn, pred_fixed@pred_elt_ids)
          return Call(succFn, succ_fixed @ succ_elts )
  *)
  
  let pred_ids : ID.t list = List.map fresh_like predFn.input_ids in
  let n_pred_fixed = List.length pred.fixed_args in 
  (*let pred_fixed_ids, pred_elt_ids = split_nth n_pred_fixed pred_ids in *)
  let pred_fixed_types, pred_elt_types = 
    split_nth n_pred_fixed (TypedSSA.input_types predFn)
  in 
  (*let succ_ids : ID.t list = List.map fresh_like succFn.input_ids in *)
  let n_succ_fixed = List.length succ.fixed_args in 
  (*let succ_fixed_ids, succ_elt_ids = split_nth n_succ_fixed succ_ids in *)
  let succ_fixed_types, succ_elt_types = 
    split_nth n_succ_fixed (TypedSSA.input_types succFn) 
  in 
  let fusedName = 
    Printf.sprintf "fused_%s_%s__%s_%s" 
      (Adverb.to_str pred.adverb) 
      (FnId.to_str pred.adverb_fn)
      (Adverb.to_str succ.adverb)
      (FnId.to_str succ.adverb_fn)
  in 
  let constructor = 
    TypedSSA.fn_buidler 
      ~namne:fusedName
      ~input_types:(pred_fixed_types @ succ_fixed_types @ pred_elt_types)
      ~outputTypes:succFn.fn_output_types  
      ~local_types:succ_elt_types 
  in 
  let new_fn : TypedSSA.fn = 
    constructor $ fun (inputs, outputs, locals) -> 
      let pred_fixed, rest = split_nth n_pred_fixed inputs in 
      let succ_fixed, pred_elts = split_nth n_succ_fixed rest in 
      let pred_fixed_ids = TypedSSA.get_ids pred_fixed in 
      let succ_fixed_ids = TypedSSA.get_ids succ_fixed in 
      let pred_elt_ids = TypedSSA.get_ids pred_elts in 
      
      [
        wrap_stmt $ 
          Set(TypedSSA.get_ids locals, 
            call pred.adverb_fn 
              (TypedSSA.output_types predFn) 
              (pred_fixed@pred_elts));
        wrap_stmt $ 
          Set(TypedSSA.get_ids outputs, 
            call succ.adverb_fn 
              (TypedSSA.output_types succFn)
              (succ_fixed @ locals));
            
      ]     
  in
  FnManager.add_typed new_fn; 
  { 
    adverb = succ.adverb; 
    adverb_fn = new_fn.fn_id; 
    fixed_args = pred.fixed_args @ succ.fixed_args;
    init = succ.init; 
    array_args = pred.array_args; 
  }


type action = Delete | Replace of exp  
type action_map = action StmtId.Map.t

let gather_actions (info:DataFlowAnalysis.env) : action_map =
  let useCounts : int ID.Map.t = 
    ID.Map.map StmtId.Set.cardinal info.consumed_by 
  in 
  let used_once id = ID.Map.find id useCounts = 1 in 
  (*
    let all_used_once idSet = ID.Set.for_all used_once idSet in 
  *)
  let rec helper stmtId adverb actions = 
    match 
      ID.Set.elements $ StmtId.Map.find_option stmtId info.produces, 
      ID.Set.elements $ StmtId.Map.find_option stmtId info.consumes 
    with 
      (* 
        FOR NOW: 
         if one of the consumed values is also used somewhere else, 
         don't try to fuse
      *)
      | Some [lhsId], Some [rhsId] when used_once rhsId -> 
        let predStmtId = ID.Map.find rhsId info.produced_by in
        let predAdverb = ID.Map.find predStmtId info.adverbs in 
        let newAdverb = fuse_adverbs predAdverb adverb in 
        StmtId.Map.add predStmtId Delete $ 
          StmtId.Map.add stmtId (Replace (Adverb newAdverb)) actions 
      | _ -> 
        (* if we don't know the inputs/outputs of a statement, skip it, 
           for now also skip if:
           (a) more than one LHS value gets produced 
           (b) any consumed ID also gets consumed by some other statement 
        *)
        actions 
  in   
  StmtId.Map.fold helper info.adverbs StmtId.Map.empty 

  
module Fusion_Rules = struct
  type context = data_flow_info 
  let init fn = gather_actions (describe_data_flow fn)
  let finalize _ _ = NoChange
  let dir = Forward
  let stmt actions stmtNode =  
    match StmtId.Map.find_option stmtNode.stmt_id actions with 
      | None -> NoChange
      | Some Delete -> Update empty_stmt
      | Some (Replace newExp) -> 
        (match stmtNode.stmt with 
          | Set ([lhs], expNode) -> 
            let rhs' = {expNode with exp = newExp} in 
            let stmtNode' = {stmtNode with stmt = Set([lhs, rhs'])} in
            Update stmtNode'
          | Set _ -> 
            failwith "[AdverbFusion] Unexpected statement with multiple LHS values"
          | _ -> NoChange 
        )
  let phi env phiNode = NoChange
  let exp env envNode = NoChange
  let value env valNode = NoChange
end

module Fusion_Rewrite = SSA_Transform.Mk(Fusion_Rules)
let fusion fn = Fusion_Rewrite.transform_fn fn 
