open SSA

type direction = Forward | Backward

type gate = (ID.t * ID.t * ID.t) list

type 'a flow_functions = { 
  merge : 'a -> 'a -> gate -> 'a; 
  split: 'a -> 'a * 'a; 
} 

(* 'a = value_info, 'b = exp_info *) 
type 'a scan_descr = { 
  scan_init_closure : closure; 
  scan_init_info : 'a list; 
  scan_combine_closure : closure; 
  scan_combine_info : 'a list; 
  scan_args : value_nodes; 
  scan_arg_info : 'a list; 
}

type 'a reduce_descr = { 
  reduce_closure : closure; 
  reduce_closure_info : 'a list; 
  reduce_args : value_nodes; 
  reduce_arg_info : 'a list;  
}

type 'a map_descr = { 
  map_closure : closure; 
  map_closure_info : 'a list; 
  map_args : value_nodes; 
  map_arg_info : 'a list; 
} 

(* 'a = value info, 'b = statement info *) 
type ('a, 'b) if_descr = {
  if_cond_val : value_node; 
  if_cond_info : 'a;
  true_branch_info: 'b; 
  true_branch_changed: bool;
  false_branch_info: 'b; 
  false_branch_changed: bool;
  if_gate: if_gate
} 

(* 'a = value info, 'b = statement info *) 
type ('a, 'b) loop_descr = { 
  loop_cond_block_info: 'b; 
  loop_cond_block_changed : bool; 
  loop_cond_val : 'a; 
  loop_body_info: 'b; 
  loop_gate : loop_gate;  
                 
} 
 
module type ANALYSIS =  sig
    type env
    type exp_info
    type value_info
    
    val dir : direction
  
    (* if env_helpers is None then perform flow insensitive analysis *)
    val flow_functions : (env flow_functions) option  
    
    (* should analysis be repeated until environment stops changing? *) 
    val iterative : bool
  
    val init : fundef -> env 
  
  (* VALUES *) 
    val var : env -> ID.t -> value_info 
    val num : env -> PQNum.num -> value_info 
    val globalfn : env -> FnId.t -> value_info
    val prim : env -> Prim.prim -> value_info  
    val str : env -> string -> value_info 
    val sym : env -> string -> value_info 
    val unit : env -> value_info 
    val lam : env -> fundef -> value_info   
  
    (* EXPRESSIONS *) 
    val values : env -> value_nodes -> value_info list -> exp_info  

    val array : env -> value_nodes -> value_info list -> exp_info 
    val app : env -> value_node -> value_info -> 
              value_nodes -> value_info list -> exp_info 
              
    val array_index : env -> value_node -> value_info -> 
                      value_nodes -> value_info list -> exp_info  
                       
    val cast : env -> DynType.t -> value_node -> value_info -> exp_info 
   
    val call : env -> typed_fn -> value_nodes -> value_info list -> exp_info 
              
    val primapp 
        : env -> typed_prim -> value_nodes -> value_info list -> exp_info
        
    val map : env -> value_info map_descr -> exp_info      
    val reduce : env -> value_info reduce_descr -> exp_info  
    val scan : env -> value_info scan_descr -> exp_info   
    
              
  (* STATEMENTS *) 
    val set : env -> ID.t list -> exp_node -> exp_info -> env option 
    val if_ : env -> (value_info, env) if_descr -> env option               
    val loop : env -> (value_info, env) loop_descr -> env option  
end

module type ENV = sig
  type t 
  val init : fundef -> t  
end 
 
module type LATTICE = sig 
  type t 
  val bottom : t  
  val combine : t -> t -> t
  val eq : t -> t -> bool 
end

module UnitLattice : LATTICE = struct
  type t = unit 
  let bottom = ()
  let combine _ _ = ()
  let eq _ _ = true
end

module TypeLattice : LATTICE = struct
  type t = DynType.t
  let bottom = DynType.BottomT
  let combine = DynType.common_type 
  let eq = (=) 
end 

module MkListLattice(L: LATTICE) : LATTICE = struct 
  type t = L.t list  
  type bottom = [] 
  let combine = List.map2 L.combine 
  let rec eq list1 list2 = match list1, list2 with 
    | [], [] -> true
    | [], _ | _, [] -> false
    | x::xs, y::ys -> L.eq x y || eq xs ys
end 

module TypeListLattice = MkListLattice(TypeLattice)

(* only information needed to specify a minimal imperative analysis
   which performs a no-op on every syntax node 
*)  

module MkAnalysis (S:ENV)(E:LATTICE)(V:LATTICE) : ANALYSIS = 
struct
  type env = S.t 
  type exp_info  = E.t 
  type value_info = V.t 
  
  let dir = Forward
  let flow_functions = None  
  let iterative = false
  
  (* ignore the function definition and just create a fresh lattice value *)
  let init _ = S.mk_default () 
  
  (* VALUES *) 
  let var _ _ = V.bottom   
  let num _ _ = V.bottom  
  let globalfn _ _ = V.bottom 
  let prim _ _ = V.bottom   
  let str _ _ = V.bottom  
  let sym _ _ = V.bottom  
  let unit _ = V.bottom  
  let lam _ _ = V.bottom    
  
  
  (* EXPRESSIONS *) 
  let array _ _ _ = E.bottom 
  let values _ _ _  = E.bottom  
  let app _ _ _ _ _  = E.bottom 
  let array_index  _ _ _ _ _  = E.bottom
  let cast _ _ _ _ = E.bottom        
  let call _ _ _ _ = E.bottom 
  let primapp _ _ _ _ = E.bottom 
  let map _ _ = E.bottom 
  let reduce _ _ = E.bottom 
  let scan _ _ = E.bottom 
  
  (* STATEMENTS *) 
  let set _ _ _ _ = None
  let if_  _ _ = None   
  let loop _ _ = None
 
end 

module MkSimpleAnalysis(S:ENV) = MkAnalysis(S)(UnitLattice)(UnitLattice)

module type EVALUATOR = functor (A : ANALYSIS) -> sig 
  val eval_block : A.env -> block -> A.env 
  val eval_stmt  : A.env -> stmt_node -> A.env 
  val eval_exp : A.env -> exp_node -> A.exp_info 
  val eval_value : A.env -> value_node -> A.value_info 
  val eval_values : A.env -> value_nodes -> A.value_info list 
end

module MkEvaluator(A : ANALYSIS) : EVALUATOR = struct
  let rec eval_block initEnv block =
    let n = block_length block in  
    let env = ref initEnv in 
    let changed = ref false in 
    match A.dir with 
    | Forward -> 
        for i = 0 to n - 1 do
          match eval_stmt !env (block_idx block i) with
            | Some env' -> env := env'; changed := true 
            | None -> () 
        done; 
        !env, !changed 
    | Backward -> 
        for i = n - 1 downto 0 do
          match eval_stmt !env (block_idx block i) with 
            | Some env' -> env := env'; changed := true
            | None -> ()
        done; 
        !env, !changed

  and eval_stmt env stmtNode = match stmtNode.stmt with 
  | Set (ids, rhs) -> 
      let rhsInfo = eval_exp env rhs in 
      A.set  env ids rhs rhsInfo
  | _ -> failwith "not yet implemented"
  and eval_exp env expNode = match expNode.exp with 
  | App(fn, args) -> 
      let fnInfo = eval_value env fn in
      let argInfos = eval_values env args in 
      A.app env fn fnInfo args argInfos 
  | _ -> failwith "not implemented"  
  and eval_value env valNode = match valNode.value with 
  | Var id -> A.var env id 
  | Num n -> A.num env n 
  | _ -> failwith "not implemented"
  and eval_values env values = List.map (eval_value env) values 

  let eval_fundef fundef = 
    let env = A.init fundef in
    let env', _ = eval_block env fundef.body in 
    env'      
end
