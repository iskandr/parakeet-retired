open Base
open SSA 
open SSA_Transform
open Printf 
open DynType 


module type REWRITE_PARAMS = sig
  val output_arity : value -> int 
  val specializer : value -> Signature.t -> fundef   
  val closureEnv : CollectPartialApps.closure_env
  val tenv : (ID.t, DynType.t) Hashtbl.t 
end  
 
module Rewrite_Rules (P: REWRITE_PARAMS) = struct 
  let get_type id = Hashtbl.find P.tenv id 
     
  let set_type id t = Hashtbl.replace P.tenv id t 
  
  let is_closure id = Hashtbl.mem P.closureEnv.CollectPartialApps.closures id
  
  let get_closure_val id = 
    Hashtbl.find P.closureEnv.CollectPartialApps.closures id
    
  let get_closure_args id = 
    Hashtbl.find P.closureEnv.CollectPartialApps.closure_args id
    
  let get_closure_arity id = 
    Hashtbl.find P.closureEnv.CollectPartialApps.closure_arity id   
  
  let infer_value_type = function 
    | Var id -> if is_closure id then DynType.BottomT else get_type id 
    | Num n -> PQNum.type_of_num n 
    | Str _ -> DynType.StrT
    | Sym _ -> DynType.SymT
    | Unit -> DynType.UnitT
    | other -> DynType.BottomT 
  
  let infer_value_node_type valNode =
    infer_value_type valNode.value   
  
  (* keeps only the portion of the second list which is longer than the first *) 
  let rec keep_tail l1 l2 = 
    if l1 = [] then l2 
    else if l2 = [] then [] 
    else keep_tail (List.tl l1) (List.tl l2) 
     
  let mk_typed_closure fnVal signature =
    match fnVal with 
    | Var id ->
      let closureArgs = get_closure_args id in  
      let closureArgTypes = List.map infer_value_node_type closureArgs in 
      let signature' = 
        Signature.prepend_input_types closureArgTypes signature 
      in
      let fnVal' = get_closure_val id in
      let fundef = P.specializer fnVal' signature' in
      {    
        closure_fn = fundef.fn_id;  
        closure_args = closureArgs;  
        closure_arg_types = closureArgTypes;  
        closure_input_types = 
          keep_tail closureArgTypes fundef.fn_input_types; 
        closure_output_types = fundef.fn_output_types;
      }
    | GlobalFn _
    | Prim _ -> 
      let fundef = P.specializer fnVal signature in 
      SSA.mk_closure fundef [] 
      
    | _ -> assert false  
                                                                                         
  let annotate_value valNode = 
    let t = infer_value_node_type valNode in  
    if t <> valNode.value_type then { valNode with value_type = t} 
    else valNode 
  
  let annotate_values vs = List.map annotate_value vs
  
  (* rewrite a value to have type t, but can't create coercion statements
     for variables of the wrong type 
   *)  
  let rewrite_value t valNode = 
    if valNode.value_type = t then valNode    
    else match valNode.value with 
      | Num n ->
        let n' = PQNum.coerce_num n t in 
        SSA.mk_num ?src:valNode.value_src ~ty:t n' 
      | Var id ->   
        let t' = get_type id in
        if t = t' then {valNode with value_type = t } 
        else failwith $ 
          Printf.sprintf 
            "Cannot rewrite %s : %s to become %s"
            (ID.to_str id)
            (DynType.to_str t')
            (DynType.to_str t)
      | Str _ -> 
        if t = DynType.StrT then 
          {valNode with value_type = DynType.StrT} 
        else assert false
      | Sym _ -> 
        if t = DynType.SymT then 
          {valNode with value_type = DynType.SymT} 
        else assert false 
      | Unit ->
        if t = DynType.UnitT then 
          {valNode with value_type = DynType.UnitT} 
        else assert false    
      | _ ->   
        failwith  $ Printf.sprintf "Can't coerce value %s to type %s"
          (SSA.value_node_to_str valNode) (DynType.to_str t)
  
  
  (* if a value needs to be coerced, then it gets added to this list. 
     It's the job of any caller to check and clear this list 
   *)
  let coercions = ref []
   
  let collect_coercions stmtNode = 
    let stmts = List.rev $ stmtNode :: !coercions in 
    coercions := []; 
    stmts 
  
  let coerce_value t valNode =
    if valNode.value_type = t then valNode
    else match valNode.value with 
      | Var id -> 
        let t' = get_type id in
        if t = t' then {valNode with value_type = t } 
        else 
        let coerceExp = SSA.mk_cast t valNode in    
        let id' =  ID.gen() in 
        set_type id' t;
        coercions :=  (SSA.mk_set [id'] coerceExp) :: !coercions;   
        SSA.mk_var ~ty:t id'
      | _ -> rewrite_value t valNode 

  let coerce_values t vs = List.map (coerce_value t) vs  

  let rewrite_adverb src adverb fnVal argNodes argTypes = 
    match adverb with 
      | Prim.Map ->
        let eltTypes = List.map DynType.peel_vec argTypes in 
        let closure = 
          mk_typed_closure fnVal (Signature.from_input_types eltTypes) 
        in 
        SSA.mk_map closure argNodes 
      | Prim.Reduce -> 
        let arity = P.output_arity fnVal in 
        let initArgs, args = List.split_nth arity argNodes in 
        let initTypes, argTypes = List.split_nth arity argTypes in
        let eltTypes = List.map DynType.peel_vec argTypes in
        let initSignature = 
          Signature.from_input_types (initTypes @ eltTypes)
        in 
        let initClosure = mk_typed_closure fnVal initSignature in
        let accTypes = initClosure.closure_output_types in      
        let reduceSignature = 
          Signature.from_types (accTypes @ eltTypes) accTypes 
        in 
        let reduceClosure = mk_typed_closure fnVal reduceSignature in 
        SSA.mk_reduce ?src initClosure reduceClosure initArgs args  
      | Prim.AllPairs -> 
        let eltTypes = List.map DynType.peel_vec argTypes in
        let eltSignature = Signature.from_input_types eltTypes in 
        assert false  
      | other -> failwith $ (Prim.adverb_to_str other) ^ " not implemented"
  
  let rewrite_app src fn argNodes : exp_node =
    let argTypes = List.map (fun v -> v.value_type) argNodes in 
    let fnVal = fn.value in  
    match fnVal with
    | Prim ((Prim.ScalarOp op) as p) -> 
      let outT = TypeInfer.infer_scalar_op op argTypes in
      if DynType.is_scalar outT then  
        SSA.mk_primapp p [outT] argNodes
      else 
        rewrite_adverb src Prim.Map fnVal argNodes argTypes 
              
    | Prim ((Prim.ArrayOp op) as p) -> 
        let outT = TypeInfer.infer_simple_array_op op argTypes in 
        SSA.mk_primapp ?src p [outT] argNodes
    | Prim (Prim.Q_Op qOp) ->  
        let outT = TypeInfer.infer_q_op qOp argTypes in 
        let prim = TypeInfer.translate_q_op qOp argTypes in 
        SSA.mk_primapp ?src prim [outT] argNodes   
    | Prim (Prim.Adverb adverb) -> 
        (match argNodes, argTypes with 
          | fn::rest, _::restTypes -> 
              rewrite_adverb src adverb fn.value rest restTypes
          | _ -> assert false
        )     
    | GlobalFn _ -> 
      let typedFundef = 
        P.specializer fnVal (Signature.from_input_types argTypes) 
      in
      SSA.mk_call 
        ?src 
        typedFundef.fn_id 
        typedFundef.fn_output_types 
        argNodes   
    | Var id -> 
      if is_closure id then
        let fnVal = get_closure_val id in 
        let closureArgs = get_closure_args id in 
        let closureArgTypes = List.map infer_value_node_type closureArgs in 
        let directArgTypes = 
          List.map (fun vNode -> vNode.value_type) argNodes 
        in 
        let types = closureArgTypes @ directArgTypes in
        let fundef = P.specializer fnVal (Signature.from_input_types types) in 
        SSA.mk_call 
          ?src
          fundef.fn_id 
          fundef.fn_output_types 
          (closureArgs @ argNodes)   
      else 
        let arrType = infer_value_node_type fn in 
        let outTypes = [DynType.slice_type arrType argTypes] in
        let arrNode = {fn with value_type = arrType} in 
        SSA.mk_primapp 
          ?src 
          (Prim.ArrayOp Prim.Index)
          outTypes 
          (arrNode::argNodes)
    | _ -> failwith $ 
             Printf.sprintf "[RewriteTyped] Unexpected function: %s"
              (SSA.value_node_to_str fn)
      
  let rewrite_exp types expNode =
    match expNode.exp, types with 
      | Arr elts, [DynType.VecT eltT] ->
          let elts' = coerce_values eltT elts in 
          {expNode with exp = Arr elts'; exp_types = types}
      | Values vs, _ ->
          let vs' = List.map2 coerce_value types vs in 
          {expNode with exp = Values vs'; exp_types = types }  
      | App (fn, args), _ -> 
          rewrite_app expNode.exp_src fn (annotate_values args)   
      | _ -> failwith $ 
               Printf.sprintf 
                 "[RewriteTyped] Type specialization for %s not implemented"
                 (SSA.exp_to_str expNode)  

  let rewrite_phi phiNode = 
    let t = Hashtbl.find P.tenv phiNode.phi_id in 
    let left = rewrite_value t phiNode.phi_left in  
    let right = rewrite_value t phiNode.phi_right in  
    {phiNode with phi_type = t; phi_left = left; phi_right = right }

  let rewrite_phi_nodes phiNodes = 
    List.map rewrite_phi phiNodes 

  let rec stmt stmtNode : stmt_node list =
    match stmtNode.stmt with
    | Set(ids, rhs) -> 
        let rhsTypes = List.map (Hashtbl.find P.tenv) ids in
        let rhs' = rewrite_exp rhsTypes rhs in
        let stmtNode' = {stmtNode with stmt = Set(ids, rhs')} in 
        collect_coercions stmtNode' 

    | SetIdx (arrayId, indices, rhs) -> failwith "setidx not implemented"
 
    | If(cond, tBlock, fBlock, phiNodes) -> 
        let cond' = coerce_value DynType.BoolT cond in
        let tBlock' = transform_block tBlock in 
        let fBlock' = transform_block fBlock in
        let phiNodes' = rewrite_phi_nodes phiNodes in 
        let stmtNode' =
          {stmtNode with stmt = If(cond', tBlock', fBlock', phiNodes')}
        in 
        collect_coercions stmtNode' 

    | WhileLoop(testBlock, testVal, body, header, exit) -> 
        let body' = transform_block body in
        let testBlock' = transform_block testBlock in
        let testVal' = coerce_value DynType.BoolT testVal in  
        let header' = rewrite_phi_nodes header in 
        let exit' = rewrite_phi_nodes exit in
        let stmtNode' =  { stmtNode with 
            stmt =WhileLoop(testBlock', testVal', body', header', exit')
        }
        in collect_coercions stmtNode' 
        
  and transform_block block =
    let buffer = ref [] in  
    let process_stmt stmtNode = 
      let newStmts = stmt stmtNode in 
      buffer := newStmts @ !buffer
    in 
    Block.iter_forward process_stmt block; 
    Block.of_list (List.rev !buffer) 
  
  and transform_fundef f =
    let body = transform_block f.body in 
    let tenv = Hashtbl.fold ID.Map.add P.tenv ID.Map.empty in 
    SSA.mk_fundef ~tenv ~input_ids:f.input_ids ~output_ids:f.output_ids ~body 
end 

let rewrite_typed ~tenv ~closureEnv ~specializer ~output_arity ~fundef =
  let module Params = struct
    let tenv = tenv
    let closureEnv = closureEnv 
    let specializer = specializer 
    let output_arity = output_arity 
  end
  in    
  let module Transform = Rewrite_Rules(Params) in 
  Transform.transform_fundef fundef   