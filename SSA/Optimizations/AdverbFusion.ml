open Base
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
module DataFlow  = struct 
  type info =  { 
    produces : ID.t list StmtId.Map.t; 
    produced_by : StmtId.t ID.Map.t;
    consumes: ID.Set.t StmtId.Map.t;
    consumed_by : StmtId.Set.t ID.Map.t; 
    adverbs :  (FnId.t, value_nodes, value_nodes) Adverb.info StmtId.Map.t; 
  }

  let empty_info = 
  { 
    produces = StmtId.Map.empty; 
    produced_by = ID.Map.empty; 
    consumes = StmtId.Map.empty; 
    consumed_by = ID.Map.empty; 
    adverbs = StmtId.Map.empty; 
  }

  let update_produced env stmtId varIds = 
    let helper oldMap id = ID.Map.add id stmtId oldMap in   
    { env with 
        produces = StmtId.Map.add stmtId varIds env.produces; 
        produced_by = List.fold_left helper env.produced_by varIds;
    }

  let update_consumed env stmtId varIds = 
    let helper oldMap id = 
      match ID.Map.find_option id oldMap with 
      | None -> ID.Map.add id (StmtId.Set.singleton stmtId) oldMap;
      | Some oldSet -> 
        ID.Map.add id (StmtId.Set.add stmtId oldSet) oldMap 
    in 
    { env with 
      consumes = 
        StmtId.Map.add stmtId (ID.Set.of_list varIds) env.consumes;
      consumed_by = List.fold_left helper env.consumed_by varIds; 
    }

  let update_data_flow_info env stmtId produced consumed = 
    let env = update_produced env stmtId produced in 
    update_consumed env stmtId consumed 
     

  let eval_value {value} = 
    match value with Var id -> Some id | _ -> None

  let rec eval_values = function 
    | v::vs -> 
      let rest = eval_values vs in 
      (match eval_value v with 
       | Some x -> x::rest 
       | None -> rest
      ) 
    | [] -> [] 

  let eval_phi_node {PhiNode.phi_left; phi_right} : ID.t list = 
    eval_values [phi_left; phi_right]

  let eval_phi_nodes phiNodes : ID.t list = 
    List.flatten (List.map eval_phi_node phiNodes)

  let eval_exp {exp} =  
    match exp with 
    | Values vs  
    | Arr vs
    | Tuple vs
    | Call (_, vs)
    | PrimApp(_, vs) -> eval_values vs
    | Cast (_, v) -> eval_values [v] 
    | Adverb { fixed_args; array_args; init; axes } -> 
      let vs = 
        fixed_args @ array_args @ axes @ (Option.default [] init )
      in 
      eval_values vs 

  let rec eval_stmt env stmtNode = 
    let stmtId = stmtNode.stmt_id in 
    let produced, consumed, blocks = match stmtNode.stmt with 
    | Set(lhsIds, expNode) ->  
      lhsIds, eval_exp expNode, []

    | SetIdx (arr, indices, rhs) -> 
      let consumed = 
        (eval_values [arr]) @ 
        (eval_values indices) @ 
        (eval_exp rhs) 
      in 
      [], consumed, []
    | If(cond, tBlock, fBlock, phiNodes) -> 
      let consumed =  
        (eval_values [cond]) @ 
        (eval_phi_nodes phiNodes) 
      in 
      [], consumed, [tBlock; fBlock]

    | WhileLoop (testBlock, test, body, phiNodes) ->
      let consumed = 
        (eval_values [test]) @ (eval_phi_nodes phiNodes)
      in 
      [], consumed, [testBlock; body] 
    in 
  
    let env : info = 
      update_data_flow_info env stmtId produced consumed 
    in
    let env : info = match stmtNode.stmt with 
    | Set(_, {exp=Adverb adverb}) -> 
      { env with 
          adverbs = StmtId.Map.add stmtId adverb env.adverbs
      } 
    | _ -> env 
    in 
    List.fold_left eval_block env blocks 
          
   and eval_block env block = 
    Block.fold_forward eval_stmt env block 

  let gather_data_flow_info {body} : info = 
    eval_block empty_info body 
end
open DataFlow


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
  assert (pred.axes = succ.axes); 
  let predFn = FnManager.get_typed_function pred.adverb_fn in 
  let succFn = FnManager.get_typed_function succ.adverb_fn in 
  (* 
     Make a new function:
        lambda(pred_fixed @ succ_fixed @ pred_elts): 
          succ_elts = Call(predFn, pred_fixed@pred_elt_ids)
          return Call(succFn, succ_fixed @ succ_elts )
  *)
  
  let n_pred_fixed = List.length pred.fixed_args in 
  let pred_fixed_types, pred_elt_types = 
    split_nth n_pred_fixed (TypedSSA.input_types predFn)
  in 
  let pred_output_types = TypedSSA.output_types predFn in 
  let n_succ_fixed = List.length succ.fixed_args in 
  let succ_fixed_types, succ_types = 
    split_nth n_succ_fixed (TypedSSA.input_types succFn) 
  in
  (* accumulators for scan and reduce *) 
  let acc_types = 
    match succ.adverb, succ.init with 
    | Adverb.Reduce, Some initArgs 
    | Adverb.Scan, Some initArgs -> 
      List.map (fun {value_type} -> value_type) initArgs
    | Adverb.Reduce, None
    | Adverb.Scan, None -> 
      [List.hd succ_types]
    | _ -> []
  in 
  let n_accs = List.length acc_types in 
  let _, succ_elt_types = split_nth n_accs acc_types in
  
  IFDEF DEBUG THEN 
    Printf.printf "Fusing %s and %s\n%!" 
      (FnId.to_str pred.adverb_fn)
      (FnId.to_str succ.adverb_fn);
    Printf.printf 
      "-- pred_fixed_types=(%s), pred_elt_types=(%s)\n%!"
      (String.concat ", "  (List.map Type.to_str pred_fixed_types))
      (String.concat ", " (List.map Type.to_str pred_elt_types));
    Printf.printf 
      "-- succ_fixed_types=(%s), succ_elt_types=(%s)\n%!"
      (String.concat ", " (List.map Type.to_str succ_fixed_types))
      (String.concat ", " (List.map Type.to_str succ_elt_types))
    ;
  ENDIF; 
  let fusedName = 
    Printf.sprintf "fused{%s_%s::%s_%s}" 
      (Adverb.to_str pred.adverb) 
      (FnId.to_str pred.adverb_fn)
      (Adverb.to_str succ.adverb)
      (FnId.to_str succ.adverb_fn)
  in 
  let constructor = 
    TypedSSA.fn_builder 
      ~name:fusedName
      ~input_types:(pred_fixed_types @ succ_fixed_types @ acc_types @ pred_elt_types)
      ~output_types:succFn.fn_output_types  
      ~local_types:pred_output_types 
  in 
  let new_fn : TypedSSA.fn = 
    constructor $ fun (inputs, outputs, locals) -> 
      let pred_fixed, rest = split_nth n_pred_fixed inputs in 
      let succ_fixed, rest = split_nth n_succ_fixed rest in 
      let accs, pred_elts = split_nth n_accs rest in 
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
              (succ_fixed @ accs @ locals));
            
      ]     
  in
  FnManager.add_typed new_fn; 
  { 
    adverb = succ.adverb; 
    adverb_fn = new_fn.fn_id; 
    fixed_args = pred.fixed_args @ succ.fixed_args;
    init = succ.init; 
    array_args = pred.array_args;
    axes = succ.axes;  
  }


type action = Delete | Replace of exp  
type action_map = action StmtId.Map.t

let gather_actions (info:DataFlow.info) : action_map =
  let useCounts : int ID.Map.t = 
    ID.Map.map StmtId.Set.cardinal info.consumed_by 
  in 
  let used_once id = ID.Map.find id useCounts = 1 in 
  (*
    let all_used_once idSet = ID.Set.for_all used_once idSet in 
  *)
  let rec helper stmtId adverb actions = 
    match 
      StmtId.Map.find_option stmtId info.produces, 
      Option.map ID.Set.elements $ 
        StmtId.Map.find_option stmtId info.consumes 
    with 
      (* 
        FOR NOW: 
         if one of the consumed values is also used somewhere else, 
         don't try to fuse
      *)
      | Some [lhsId], Some [rhsId] when 
        ID.Map.mem rhsId info.produced_by && 
        used_once rhsId -> 
        let predStmtId : StmtId.t = 
          ID.Map.find rhsId info.produced_by 
        in
        let predAdverb = 
          StmtId.Map.find predStmtId info.adverbs 
        in 
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
  type context = action_map  
  let init fn = gather_actions (gather_data_flow_info fn)
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
            let stmtNode' = 
              {stmtNode with stmt = Set([lhs], rhs')} in
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
