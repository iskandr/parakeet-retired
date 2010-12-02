| Var of ID.t
  | GlobalFn of FnId.t  
  | Num of PQNum.num 
  | Str of string
  | Sym of string
  | Unit
  | Prim of Prim.prim
  | Lam of fundef
  (* place holder for initial values of reductions, 
     which for now are not supported but still need to 
     be given sensible types
  *) 
  | Stream of value_node * DynType.t  

type ('a,'b) value_logic = { 
  var : 'a -> 'b -> ID.t -> (SSA.value * 'b * bool); 
  num : 'a -> 'b -> PQNUm.num -> (SSA.value * 'b * bool); 
  str : 'a -> 'b -> string -> (SSA.value * 'b * bool); 
  sym : 'a -> 'b -> string -> (SSA.value * 'b * bool); 
  lam : 'a -> 'b -> SSA.fundef -> (SSA.value * 'b * bool); 
}

let default_value_logic = { 
  var = (fun sInfo vInfo id -> SSA.Var id, vInfo, false); 
  num = (fun sInfo vInfo num -> SSA.Num num, vInfo, false); 
  str = (fun sInfo vInfo s -> SSA.Str s, vInfo, false); 
  sym = (fun sInfo vInfo s -> SSA.Sym s, vInfo, false); 
  lam = failwith "lambda unexpected"
} 


type ('a,'b) exp_logic = {
  app : 
    'a -> 'b -> SSA.value_node -> SSA.value_node list -> (SSA.exp * 'b * bool);
  array_index : 
    'a -> 'b -> SSA.value_node -> SSA.value_node list -> (SSA.exp * 'b * bool); 
  arr : 
    'a -> 'b -> SSA.value_node list -> (SSA.exp * 'b * bool); 
  values : 
    'a -> 'b -> SSA.value_node list -> (SSA.exp * 'b * bool); 
  cast : 
    'a -> 'b -> DynType.t * SSA.value_node -> (SSA.exp * 'b * bool); 
  call : unit; 
  primapp : unit; 
  map : unit; 
  reduce: unit; 
  scan : unit; 
}  
     
let rec rewrite_stmt 
          fixedInfo 
          volatileInfo 
          stmtLogic
          expLogic 
          valLogic 
          stmtNode = 
  match stmtNode.stmt with  
  | Set (ids, rhs) ->
      let rhs', vInfo, rhsChanged = 
        expLogic fixedInfo volatileInfo valLogic rhs 
      in 
      stmtLogic.set fixedInfo vInfo
        
  | SetIdx of ID.t * (value_node list) * value_node
  | If of value_node * block * block * if_gate
  | WhileLoop of block * ID.t * block * loop_gate  

and  exp = 
   
and typed_fn = { 
  fn_id : FnId.t; 
  fn_input_types : DynType.t list; 
  fn_output_types : DynType.t list;   
} 
and typed_prim = { 
  prim_input_types : DynType.t list; 
  prim_output_types : DynType.t list; 
  prim: Prim.prim; 
} 
and closure = {   
  closure_fn: FnId.t; 
  closure_args: value_node list; 
  closure_arg_types: DynType.t list; 
  closure_input_types:DynType.t list; 
  closure_output_types: DynType.t list 
} 
and exp_node = { 
  exp: exp; 
  exp_src : source_info option;
  (* because a function applicatin might return multiple values,*)
  (* expressions have multiple types *)  
  exp_types : DynType.t list; 
} 
and value_node = { 
  value_type : DynType.t;
  value_src : source_info option; 
  value : value 
}  
and value = 
  