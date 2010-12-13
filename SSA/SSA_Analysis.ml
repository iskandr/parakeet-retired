open SSA

type direction = Forward | Backward

type gate = (ID.t * ID.t * ID.t) list

type 'a flow_functions = { 
  merge : 'a -> 'a -> gate -> 'a; 
  split: 'a -> 'a * 'a; 
} 

(* like the exp type in SSA, but replacing value_node with generic 'a *)
(* type parameter used to fill in different value_info types *)  
type 'a open_exp = 
  | AppInfo of 'a * 'a list 
  | CallInfo of 'a list 
  | MapInfo of 'a list * 'a list  
  | ScanInfo of 'a list * 'a list * 'a list
  | ReduceInfo of 'a list * 'a list * 'a list  
  | ValuesInfo of 'a list
  | ArrayInfo of 'a list

(* 'a = value_info, 'b = exp_info, 'c = env *) 
type ('a,'b,'c) open_stmt = 
  (* rhs exp_info *)  
  | SetInfo of 'b  
  (* if condition, true branch env, false branch env *) 
  | IfInfo of 'a * 'c * 'c
  (* cond body, cond value, loop body *) 
  | LoopInfo of 'c * 'a * 'c  
    
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
  
    val value : env -> value_node -> value_info 
    val exp : env -> exp_node -> value_info open_exp -> exp_info  
    val stmt 
      : env -> stmt_node -> (value_info, exp_info, env) open_stmt -> env option  
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

module UnitLattice  = struct
  type t = unit 
  let bottom = ()
  let combine _ _ = ()
  let eq _ _ = true
end

module TypeLattice  = struct
  type t = DynType.t
  let bottom = DynType.BottomT
  let combine = DynType.common_type 
  let eq = (=) 
end 

module MkListLattice(L: LATTICE)  = struct 
  type t = L.t list  
  let bottom = [] 
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

module MkAnalysis (S:ENV)(E:LATTICE)(V:LATTICE)  = 
struct
  type env = S.t 
  type exp_info  = E.t 
  type value_info = V.t 
  
  let dir = Forward
  let flow_functions = None  
  let iterative = false
  
  (* ignore the function definition and just create a fresh lattice value *)
  let init fundef = S.init fundef
  
  let value _ _ = V.bottom 
  let exp _ _ _ = E.bottom 
  let stmt _ _ _= None
end 

module MkSimpleAnalysis(S:ENV) = MkAnalysis(S)(UnitLattice)(UnitLattice)

(*
module type EVALUATOR = functor (A : ANALYSIS) -> sig 
  val eval_block : A.env -> block -> A.env 
  val eval_stmt  : A.env -> stmt_node -> A.env 
  val eval_exp : A.env -> exp_node -> A.exp_info 
  val eval_value : A.env -> value_node -> A.value_info 
  val eval_values : A.env -> value_nodes -> A.value_info list
  val eval_fundef : fundef -> A.env  
end
*)

module MkEvaluator(A : ANALYSIS) = struct 
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

  and eval_stmt env stmtNode = 
    let info = match stmtNode.stmt with 
    | Set (ids, rhs) -> SetInfo (eval_exp env rhs) 
    | _ -> failwith "not yet implemented"
    in A.stmt env stmtNode info 
  and eval_exp env expNode = 
    let info = match expNode.exp with 
      | App(fn, args) -> 
        let fnInfo = eval_value env fn in
        let argInfos = eval_values env args in
        AppInfo (fnInfo, argInfos)
      | _ -> failwith "not implemented"     
    in A.exp env expNode info  
   
  and eval_value = A.value  
  and eval_values env values = List.map (eval_value env) values 

  let eval_fundef fundef = 
    let env = A.init fundef in
    let env', _ = eval_block env fundef.body in 
    env'      
end
