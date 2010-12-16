open Base
open SSA 
open SSA_Transform
open Printf 
open DynType 

(* Given a type environment and a list of annotated but untyped stmtNodes *)
(* return a list of typed statement nodes and the modified type environment*)
(* which might contain the types produced by any inserted coercions. *)
(* However, don't try to "coerce" functions since function values *)
(* are assumed to have been correctly specialized by the annotator.  *)

module type REWRITE_PARAMS = sig
  val specializer : value -> DynType.t list -> FnId.t  
  val closureEnv : CollectPartialApps.closure_env
  val tenv : (ID.t, DynType.t) Hashtbl.t 
end  
 
module Rewrite_Rules (P: REWRITE_PARAMS) = struct 
  type env = (ID.t, DynType.t) Hashtbl.t 
  let init _ = P.tenv    
  let dir = Forward

  let value tenv valNode =  
      let t = match valNode.value with 
      | Var id -> Hashtbl.find tenv id 
      | Num n -> PQNum.type_of_num n 
      | Str _ -> DynType.StrT
      | Sym _ -> DynType.SymT
      | Unit -> DynType.UnitT
      | _ -> 
        failwith $ "unexpected SSA value: %s" ^ (SSA.value_node_to_str valNode)
      in     
      if t <> valNode.value_type then Update { valNode with value_type = t} 
      else NoChange  
    
  (* all modifications to expressions happen in the top-down coerce_exp fn *)  
  let exp tenv expNode = NoChange 
    
  (* since the SSA_Transform framework only allows for bottom-up transformations
     we need to specify these coerce_* functions for type information 
     to flow from bindings down to values 
  *) 
  let coerce_value valNode t = 
    match valNode.value with 
    | Num n ->
      let n' = PQNum.coerce_num n t in 
      let valNode' = SSA.mk_num ~value_src:valNode.value_src ~ty:t n' in 
      valNode', [], true
    | Var id ->   
      let coerceExp = SSA.mk_cast t valNode in    
      let id' =  ID.gen() in 
      add_type id' t;  
      SSA.mk_var ~ty:t id', [SSA.mk_set [id'] coerceExp], true  
    | _ -> 
      failwith  $ Printf.sprintf "Can't coerce value %s to type %s"
        (SSA.value_node_to_str valNode) (DynType.to_str t)
        
       
  let rec coerce_values valNodes ts stmtsRef changedRef =
    match valNodes, ts with 
    | valNode::restNodes, t::restTypes ->
        let rest' = coerce_values restNodes restType in
        if valNode.value_type <> t then  
          let valNode', stmts, currChanged = coerce_value valNode t in
          begin 
            stmtsRef := stmts @ !stmtsRef;
            changedRef := !changedRef || currChanged;
            valNode'::rest'
          end 
        else valNode::rest'  
    | [], [] -> [] 
    | _ -> assert false
  
  let coerce_exp expNode types = 
    (* instead of repeatedly creating tuples, get a slight performance 
       boost by passing in references to 'stmts' and the 'changed' flag
    *)
    let changedRef = ref false in 
    let stmtsRef = ref [] in 
    let exp' = match expNode.exp, types with 
      | Arr elts, [DynType.VecT eltT] ->
        let coerceTypes = List.map (fun _ ->  eltT) elts in
        let elts' = coerce_values elts types stmtsRef changedRef in
        if !changedRef then  Arr elts' else expNode.exp  
      | Values vs, _ -> 
          let vs' = coerce_values vs types stmtsRef changedRef in
          if !changedRef then Values vs' else expNode.exp  
    in 
    {expNode with exp=exp'; exp_types = types}, !stmtsRef, !changedRef         
  
  (* convert the types hashtbl to a ID.Map.t, store it as the function's 
     type environment 
   *)  
  let fundef typesHash f = 
    Update {f with tenv = Hashtbl.fold ID.Map.add typesHash ID.Map.empty } 
  
  (* all the exp/value child nodes have already been processed once, 
     here we revisit some of them to enforce top-down constraints such as
     - all elements of an array must be of the same type 
  *) 
  let stmt tenv stmtNode = match stmtNode.stmt with
    | Set(ids, rhs) -> 
        let rhsTypes = List.map (Hashtbl.find tenv) ids in 
        let rhs', stmts, changed = coerce_exp rhs rhsTypes in 
        if changed then 
          let stmtNode' = {stmtNode with stmt = Set(ids, rhs') } in 
          Some (stmts @ [stmtNode'])
        else None 
    | _ -> None 

end 

let rewrite_typed tenv closureEnv specializer fundef =
  let module Params = struct
    let tenv = tenv
    let closureEnv = closureEnv 
    let specializer = specializer 
  end
  in    
  let module Transform = SSA_Transform.MkTransformation(Rewrite_Rules(Params))
  in
  Transform.rewrite_fundef fundef  