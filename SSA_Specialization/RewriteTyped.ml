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
  let dir = Forward
  type context = unit
  let init _ = ()
 (* convert the types hashtbl to a ID.Map.t, store it as the function's 
    type environment 
  *)  
  let finalize _ f = 
    Update {f with 
      tenv = Hashtbl.fold ID.Map.add P.tenv ID.Map.empty;
      fn_input_types = 
        List.map (Hashtbl.find P.tenv) f.input_ids;
      fn_output_types = 
        List.map (Hashtbl.find P.tenv) f.output_ids;   
    } 

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
    IFDEF DEBUG THEN 
      Printf.printf "\tRewriteTyped::infer_value_node_type %s\n"
        (SSA.value_node_to_str valNode); 
    ENDIF; 
    infer_value_type valNode.value   
  
  (* keeps only the portion of the second list which is longer than the first *) 
  let rec keep_tail l1 l2 = 
    if l1 = [] then l2 
    else if l2 = [] then [] 
    else keep_tail (List.tl l1) (List.tl l2) 
     
  let mk_typed_closure fnVal signature =
    IFDEF DEBUG THEN  
      Printf.printf "\tRewriteTyped::mk_typed_closure %s : %s\n" 
        (SSA.value_to_str fnVal) (Signature.to_str signature)
      ;
    ENDIF;   
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
      { 
        closure_fn = fundef.fn_id;  
        closure_args = [];  
        closure_arg_types = [];  
        closure_input_types = fundef.fn_input_types; 
        closure_output_types = fundef.fn_output_types;   
      }
    | _ -> assert false  
                                                                                         
  let annotate_value valNode = 
    let t = infer_value_node_type valNode in  
    if t <> valNode.value_type then Update { valNode with value_type = t} 
    else NoChange  

  let coerce_value t valNode =
    IFDEF DEBUG THEN 
      Printf.printf "RewriteTyped::Coerce Value %s\n"
        (SSA.value_node_to_str valNode); 
    ENDIF; 
    if valNode.value_type = t then NoChange 
    else match valNode.value with 
      | Num n ->
        let n' = PQNum.coerce_num n t in 
        Update (SSA.mk_num ?src:valNode.value_src ~ty:t n')
      | Var id ->   
        let t' = get_type id in
        if t = t' then Update {valNode with value_type = t }
        else  
        let coerceExp = SSA.mk_cast t valNode in    
        let id' =  ID.gen() in 
        set_type id' t;  
        UpdateWithStmts(SSA.mk_var ~ty:t id', [SSA.mk_set [id'] coerceExp])
      | Str _ -> 
        if t = DynType.StrT then 
          Update {valNode with value_type = DynType.StrT}
        else assert false
      | Sym _ -> 
        if t = DynType.SymT then 
          Update {valNode with value_type = DynType.SymT}
        else assert false 
      | Unit ->
        if t = DynType.UnitT then 
          Update{valNode with value_type = DynType.UnitT}
        else assert false    
      | _ ->   
        failwith  $ Printf.sprintf "Can't coerce value %s to type %s"
          (SSA.value_node_to_str valNode) (DynType.to_str t)
  
  let rewrite_adverb src adverb fnVal argNodes argTypes = match adverb with 
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
        
      | other -> failwith $ (Prim.adverb_to_str other) ^ " not implemented"
  
  let rewrite_app src fn argNodes : exp_node =
    let argTypes = List.map (fun v -> v.value_type) argNodes in 
    let fnVal = fn.value in  
    match fnVal with
    | Prim ((Prim.ScalarOp op) as p) -> 
      let outT = TypeInfer.infer_scalar_op op argTypes in
      let commonT = DynType.fold_type_list argTypes in 
      if DynType.is_scalar outT then  
        SSA.mk_primapp p [outT] argNodes
      else 
        rewrite_adverb src Prim.Map fnVal argNodes argTypes 
              
    | Prim ((Prim.ArrayOp op) as p) -> 
        let outT = TypeInfer.infer_simple_array_op op argTypes in 
        SSA.mk_primapp ?src p [outT] argNodes
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
        let nIndices = List.length argNodes in 
        let arrType = infer_value_node_type fn in 
        let outTypes = [DynType.slice_type arrType argTypes] in
        let arrNode = {fn with value_type = arrType} in 
        SSA.mk_primapp 
          ?src 
          (Prim.ArrayOp Prim.Index)
          outTypes 
          (arrNode::argNodes)
        
      
      
  let rewrite_exp 
        (processVal 
          : (value_node -> value_node update) -> value_node -> value_node) 
        types 
        expNode =
    let annotate_values = List.map (processVal annotate_value) in 
    match expNode.exp, types with 
      | Arr elts, [DynType.VecT eltT] ->
        let elts' = 
          List.map (fun elt -> processVal (coerce_value eltT) elt) elts
        in  
        Update {expNode with exp = Arr elts'; exp_types = types }    
      | Values vs, _ ->
        let vs' = 
          List.map2 (fun v t -> processVal (coerce_value t) v) vs types
        in 
        Update {expNode with exp = Values vs'; exp_types = types } 
      | App (fn, args), _ -> 
        Update (rewrite_app expNode.exp_src fn (annotate_values args))       
    
  let rec stmt context helpers stmtNode =
    match stmtNode.stmt with
    | Set(ids, rhs) -> 
        let rhsTypes = List.map (Hashtbl.find P.tenv) ids in
        let rhs' = 
          helpers.transform_exp (rewrite_exp helpers.transform_value rhsTypes) rhs 
        in 
        Update {stmtNode with stmt = Set(ids, rhs')}
    | SetIdx (arrayId, indices, rhs) -> failwith "setidx not implemented"
    | If(cond, tBlock, fBlock, merge) -> 
        let cond' = helpers.transform_value (coerce_value DynType.BoolT) cond in
        let tBlock' = helpers.transform_block (stmt context) tBlock in 
        let fBlock' = helpers.transform_block (stmt context) fBlock in
        (* TODO: deal with phi nodes 
        let mergeBlock' = helpers.transform_block (stmt context) mergeBlock in
        *)  
        Update {stmtNode with stmt = If(cond', tBlock', fBlock', merge)}
    | WhileLoop(testBlock, testVal, body, header, exit) -> 
        let body' = helpers.transform_block (stmt context) body in
        (* TODO: deal with phi nodes 
        let gates' = { 
          loop_exit =  helpers.transform_block (stmt context) gates.loop_header;
          loop_header = helpers.transform_block (stmt context) gates.loop_exit
        } 
        *)
        let testBlock' = helpers.transform_block (stmt context) testBlock in
        let testVal' = 
          helpers.transform_value (coerce_value DynType.BoolT) testVal
        in  
        Update { stmtNode with 
          stmt =WhileLoop(testBlock', testVal', body', header, exit)
        } 
end 

let rewrite_typed ~tenv ~closureEnv ~specializer ~output_arity ~fundef =
  let module Params = struct
    let tenv = tenv
    let closureEnv = closureEnv 
    let specializer = specializer 
    let output_arity = output_arity 
  end
  in    
  let module Transform = SSA_Transform.MkCustomTransform(Rewrite_Rules(Params))
  in 
  IFDEF DEBUG THEN
    Printf.printf "Rewrite Typed (before): %s \n%!" (SSA.fundef_to_str fundef);
  ENDIF;
  let fundef, _ = Transform.transform_fundef fundef in fundef   