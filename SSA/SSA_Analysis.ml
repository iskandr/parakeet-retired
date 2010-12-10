open SSA

type direction = Forward | Backward

type gate = (ID.t * ID.t * ID.t) list

type 'a flow_functions = { 
  merge : 'a -> 'a -> gate -> 'a; 
  split: 'a -> 'a * 'a; 
} 

(* 'a = value_info, 'b = exp_info *) 
type 'a scan_info = { 
  scan_init_closure : closure; 
  scan_init_info : 'a list; 
  scan_combine_closure : closure; 
  scan_combine_info : 'a list; 
  scan_args : value_nodes; 
  scan_arg_info : 'a list; 
}

type 'a reduce_info = { 
  reduce_closure : closure; 
  reduce_closure_info : 'a list; 
  reduce_args : value_nodes; 
  reduce_arg_info : 'a list;  
}

type 'a map_info = { 
  map_closure : closure; 
  map_closure_info : 'a list; 
  map_args : value_nodes; 
  map_arg_info : 'a list; 
} 

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
  let bottom = [] 
  let combine = List.map2 L.combine 
  let rec eq list1 list2 = match list1, list2 with 
    | [], [] -> true
    | [], _ | _, [] -> false
    | x::xs, y::ys -> L.eq x y || eq xs ys
end 

module TypeListLattice = MkListLattice(TypeLattice) 
 
module type SEMANTICS = 
  functor (S : LATTICE) -> 
  functor (E : LATTICE) ->
  functor (V : LATTICE) -> sig  
    val dir : direction
  
    (* if env_helpers is None then perform flow insensitive analysis *)
    val flow_functions : (S.t flow_functions) option  
    
    (* should analysis be repeated until environment stops changing? *) 
    val iterative : bool
  
    val init : fundef -> S.t 
  
  (* VALUES *) 
    val var : S.t -> ID.t -> V.t  
    val num : S.t -> PQNum.num -> V.t 
  
    (* EXPRESSIONS *) 
    val values : S.t -> value_nodes -> V.t list -> E.t  

    val array : S.t -> value_nodes -> V.t list -> E.t 
    val app : S.t -> value_node -> V.t -> value_nodes -> V.t list -> E.t 
              
    val array_index : S.t -> value_node -> V.t -> value_nodes -> V.t list -> E.t  
                       
    val cast : S.t -> DynType.t -> value_node -> V.t -> E.t 
   
    val call : S.t -> typed_fn -> value_nodes -> V.t list -> E.t 
              
    val primapp : S.t -> typed_prim -> value_nodes -> V.t list -> E.t
    val map : S.t -> V.t map_info -> E.t      
    val reduce : S.t -> V.t reduce_info -> E.t  
    val scan : S.t -> V.t scan_info -> E.t   
    
              
  (* STATEMENTS *) 
  val set : env -> ID.t list -> exp_node -> exp_info -> env option
  
  val if_ : env -> condVal:value_node -> condInfo:value_info -> 
              trueEnv:env -> trueChanged:bool -> 
              falseEnv:env -> falseChanged:bool -> if_gate -> env option
              
  val while_ : env -> ?condBlockEnv:env -> ?condBlockChanged:bool -> 
                 condVal:value_node -> bodyEnv:env -> bodyChanged:bool -> 
                 loop_gate -> env option  
end

(* only information needed to specify a minimal imperative analysis
   which performs a no-op on every syntax node 
*)  
module type ENV = sig 
  type env 
  val init : fundef -> env
end 

module MakeSimpleAnalysis(E : ENV) : ANALYSIS = struct
  type env = A.env  
  type exp_info = unit 
  type value_info = unit
  
  let dir = Forward
  let flow_functions = None  
   
  let iterative = false
  
  let  init = A.init 
  
  (* VALUES *) 
  let var env id = () 
  let num env n = () 
  
  (* EXPRESSIONS *) 
  let app env fNode fInfo argNode argInfos = ()
  let array env  vNodes vInfos = ()
  let values env vNode vInfos = () 
  
  (* STATEMENTS *) 
  let set env ids rhsNode rhsInfo = None
  let if_ env 
          ~condVal ~condInfo 
          ~trueEnv ~trueChanged ~falseEnv ~falseChanged ifGate = None 
              
  let while_ env ~condBlockEnv ~condBlockChanged ~condVal
                 ~bodyEnv ~bodyChanged loopGate = None 
end 


module type EVALUATOR = functor (A : ANALYSIS) -> sig 
  val eval_block : A.env -> block -> A.env 
  val eval_stmt  : A.env -> stmt_node -> A.env 
  val eval_exp : A.env -> exp_node -> A.exp_info 
  val eval_value : A.env -> value_node -> A.value_info 
  val eval_values : A.env -> value_nodes -> A.value_info list 
end

module MakeEvaluator(A : ANALYSIS) : EVALUATOR = struct
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
