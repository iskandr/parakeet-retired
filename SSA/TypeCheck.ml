(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open Printf 

type errors = (SrcInfo.t option * string) Queue.t 
type tenv = Type.t ID.Map.t 


let rec check_stmt 
          (errorLog : errors) 
          (tenv : tenv) 
          (defined : ID.Set.t) 
          (stmtNode:stmt_node) 
          : ID.Set.t =
  let err msg = Queue.add (stmtNode.stmt_src, msg) errorLog in
  match stmtNode.stmt with 
  | Set (ids, rhs) ->
      check_exp errorLog tenv defined rhs; 
      let type_exists (id :ID.t) : bool = 
        let found = ID.Map.mem id tenv in  
        if not found then 
          err $ Printf.sprintf 
            "no type found for variable %s on lhs of assignment" 
            (ID.to_str id)
        ;
        found
      in   
      if List.for_all type_exists ids then ( 
        let lhsTypes = List.map (fun id -> ID.Map.find id tenv) ids in 
        let rhsTypes = rhs.exp_types in 
        if lhsTypes <> rhsTypes then (
          let lhsCount = List.length ids in
          let rhsCount = List.length rhs.exp_types in
          err $ sprintf 
            "type error in assignment: %s type %s, but rhs %s type %s %s"
            (if lhsCount > 1 then "identifiers have" else  "identifier has")
            (Type.type_list_to_str lhsTypes)
            (if rhsCount > 1 then  "values have" else "value has")
            (Type.type_list_to_str rhsTypes)
            (if lhsCount <> rhsCount then "(arity mismatch)" else "")
         )
      );
      ID.Set.add_list ids defined 
  | SetIdx (arrId, indices, rhs) -> 
      if not $ ID.Set.mem arrId defined then 
        err $ sprintf 
          "attempting to set array index on undefined variable %s"
          (ID.to_str arrId) 
      else (
        let lhsType = ID.Map.find arrId tenv in 
        if Type.is_scalar lhsType then 
          err $ Printf.sprintf "cannot index into scalar %s" (ID.to_str arrId)
      ); 
      check_value_list errorLog tenv defined indices;
      check_value errorLog tenv defined rhs;
      defined  
  | If (test, tBlock, fBlock, phiNodes) ->
      check_value errorLog tenv defined test;
      let _ = check_block errorLog tenv defined tBlock in  
      let _ = check_block errorLog tenv defined fBlock in  
      let phiIds = List.map (fun phiNode -> phiNode.phi_id) phiNodes in 
      ID.Set.add_list phiIds defined 
      
  | WhileLoop (testBlock, testVal, body, phiNodes) -> 
      let phiIds = List.map (fun phiNode -> phiNode.phi_id) phiNodes in 
      let defined' = ID.Set.add_list phiIds defined in 
      let _ = check_block errorLog tenv defined'  testBlock in 
      check_value errorLog tenv defined' testVal; 
      let _ = check_block errorLog tenv defined' body in  
      (* TODO: check phi node types *)
      defined' 
and check_exp errorLog tenv (defined : ID.Set.t) (expNode : exp_node) : unit = 
  let err msg = Queue.add (expNode.exp_src, msg) errorLog in
  match expNode.exp with 
  | App (fn, args) -> 
      check_value errorLog tenv defined fn;  
      check_value_list errorLog tenv defined args; 
      let argTypes = List.map (fun v -> v.value_type) args in
      let fnType = fn.value_type in  
      if Type.is_function fnType then (
        let expectedTypes =  Type.fn_input_types fnType in
        (* HACK: ignore the case where there are too few args so our primitive
           closure implementation doesn't break 
        *) 
        if List.length expectedTypes <= List.length argTypes then (
          if expectedTypes <> argTypes then 
            err $ sprintf  
              "type mismatch in function application\n\
              \t  - expected: %s\n\t  - received: %s"
              (Type.type_list_to_str expectedTypes)
              (Type.type_list_to_str argTypes)
            ;
          let retTypes = Type.fn_output_types fnType in 
          if retTypes <> expNode.exp_types then 
            err $ sprintf 
              "function returns %s but context expects to receive %s"
              (Type.type_list_to_str retTypes)
              (Type.type_list_to_str expNode.exp_types)
        )
      )
      else err $  
             sprintf "expected a function type, received: %s"
               (Type.to_str fnType)
   
  | Values vs 
  | Arr vs -> check_value_list errorLog tenv defined vs
  | Cast (t, v) -> 
      check_value errorLog tenv defined v; 
      if expNode.exp_types <> [t] then 
        err $ sprintf "context expects type %s but cast is to type %s"
          (Type.type_list_to_str expNode.exp_types)
          (Type.to_str t)  
  | Call (typedFn, args) -> ()
  | PrimApp (typedPrim, args) -> ()  
  | Map (closure,args) -> ()
  | Reduce (initClos, reduceClos, initArgs, args) -> ()   
  | Scan (initClos, scanClos, initArgs, args) -> ()
and check_value (errorLog:errors)(tenv:tenv) (defined : ID.Set.t) vNode : unit = 
 let err msg = Queue.add (vNode.value_src, msg) errorLog in
  match vNode.value with 
  | Var id -> 
      if not $ ID.Set.mem id defined then 
        err $ sprintf "attempting to use undefined variable %s" (ID.to_str id)
      else if not $ ID.Map.mem id tenv then 
        err $ sprintf "type not found for %s" (ID.to_str id)
      else 
        let t = ID.Map.find id tenv in ( 
        if t <> vNode.value_type then 
          err $ sprintf 
            "type annotation %s for variable %s does not match its type %s"
            (Type.to_str vNode.value_type)
            (ID.to_str id)
            (Type.to_str t)
        )
  | Num _ ->  
      if not $ Type.is_number vNode.value_type then
        err "number annotated with non-numeric type" 
  | Prim _
  | GlobalFn _ -> 
      if not $ Type.is_function vNode.value_type then 
        err "expected function annotation"  
  | Sym _ | Str _ | Unit  -> ()
and check_value_list errorLog tenv defined values = 
  List.iter (check_value errorLog tenv defined) values
and check_block (errorLog : errors) (tenv : Type.t ID.Map.t) defined block = 
  Block.fold_forward
    (fun accDefined stmtNode -> check_stmt errorLog tenv accDefined stmtNode) 
    defined
    block 
and check_fundef ?(errorLog=Queue.create()) fundef =
  let defined = ID.Set.of_list fundef.input_ids in  
  let _ = check_block errorLog fundef.tenv defined fundef.body in 
  errorLog

let err_printer = function 
  | (None, msg) -> Printf.printf "  * %s\n" msg
  | (Some src, msg) -> Printf.printf "  * %s : %s\n" msg "[source]" 

let print_all_errors errorLog =  Queue.iter err_printer errorLog 
  
