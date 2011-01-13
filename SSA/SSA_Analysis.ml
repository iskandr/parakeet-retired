open SSA
open Base


type direction = Forward | Backward
      
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

(* can be reused for most value lattices when environment is a hash *) 
let mk_hash_merge combine outEnv outId xEnv xId yEnv yId =  
  let x = Hashtbl.find xEnv xId in
  let y = Hashtbl.find yEnv yId in  
  let newVal = combine x y in 
    if Hashtbl.mem outEnv outId then 
      let oldVal = Hashtbl.find outEnv outId in  
      if oldVal = newVal then None 
      else (Hashtbl.add outEnv outId (combine oldVal newVal);  Some outEnv)
    else (Hashtbl.add outEnv outId newVal; Some outEnv)
    
(* can be reused for most value lattices when environment is a tree map *) 
let mk_map_merge combine outEnv outId xEnv xId yEnv yId = 
  let x = ID.Map.find xId xEnv in 
  let y = ID.Map.find yId yEnv in
  let newVal = combine x y in 
  if ID.Map.mem outId outEnv then 
    let oldVal = ID.Map.find outId outEnv in 
    if oldVal = newVal then None 
    else Some (ID.Map.add outId (combine oldVal newVal) outEnv)
  else Some (ID.Map.add outId newVal outEnv)   
        
module type ANALYSIS =  sig
    type env
    type exp_info
    type value_info
    
    val dir : direction
  
    val flow_split : env -> env * env  
    val flow_merge : env -> ID.t -> env -> ID.t -> env -> ID.t -> env option   
    
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
  let flow_split env = env, env  
  let flow_merge _ _ _ _ _ _ = None   
  
  let iterative = false
  
  (* ignore the function definition and just create a fresh lattice value *)
  let init fundef = S.init fundef
  
  let value _ _ = V.bottom 
  let exp _ _ _ = E.bottom 
  let stmt _ _ _= None
  
  let eval_set 
        (_:env) 
        (_:SourceInfo.source_info) 
        ~(ids:ID.t list) 
        ~(rhs:SSA.exp_node)
        ~(rhsInfo:E.t) = None 
  
  let eval_if 
        (_:env) 
        (_:SourceInfo.source_info)
        ~(cond:SSA.value_node)
        ~(tBlock:SSA.block)
        ~(fBlock:SSA.block)
        ~(gate:SSA.if_gate)
        ~(condInfo:V.t)
        ~(tEnv:env)
        ~(fEnv:env) = None 
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

  and eval_stmt env stmtNode = match stmtNode.stmt with 
    | Set (ids, rhs) -> A.stmt env stmtNode (SetInfo (eval_exp env rhs)) 
    | If(cond, tBlock, fBlock, gate) -> 
        let condInfo = eval_value env cond in
        let trueEnv, falseEnv = A.flow_split env in 
        let trueEnv', trueChanged = eval_block trueEnv tBlock in 
        let falseEnv', falseChanged = eval_block falseEnv fBlock in
        let mergeChanged = ref false in 
        let folder env outId trueId falseId =
          match A.flow_merge env outId trueEnv' trueId falseEnv' falseId with
            | Some env' -> mergeChanged := true; env'  
            | None -> env
        in       
        let combinedEnv = 
          List.fold_left3 
            folder 
            env 
            gate.if_output_ids 
            gate.true_ids 
            gate.false_ids
        in 
        let info = IfInfo (condInfo, trueEnv', falseEnv') in 
        (match A.stmt combinedEnv stmtNode info with 
          | None ->
            (* since None indicates no change was made we can only*)
            (* propagate it if none of the operations on child nodes*)
            (* also didn't involve changes *)
            if trueChanged || falseChanged || !mergeChanged 
            then Some combinedEnv else None     
         | modifiedEnv -> modifiedEnv
         )   
    | _ -> failwith "not yet implemented"
     
  and eval_exp env expNode = 
    let info = match expNode.exp with 
      | App(fn, args) -> 
        let fnInfo = eval_value env fn in
        let argInfos = eval_values env args in
        AppInfo (fnInfo, argInfos)
       
      | Call(typedFn, args) -> CallInfo (eval_values env args) 
      | Map(closure, args) -> 
        let closureInfos = eval_values env closure.closure_args in 
        let argInfos = eval_values env args in  
        MapInfo (closureInfos, argInfos)
          
      | Scan(initClosure, reduceClosure, args) -> 
        let initInfos = eval_values env initClosure.closure_args in 
        let reduceInfos = eval_values env reduceClosure.closure_args in 
        let argInfos = eval_values env args in 
        ScanInfo (initInfos, reduceInfos, argInfos)
     | ReduceInfo of 'a list * 'a list * 'a list  
  | ValuesInfo of 'a list
  | ArrayInfo of 'a list
      | _ -> failwith "not implemented"     
    in A.exp env expNode info  
   
  and eval_value = A.value  
  and eval_values env values = List.map (eval_value env) values 

  let eval_fundef fundef = 
    let env = A.init fundef in
    let env', _ = eval_block env fundef.body in 
    env'      
end
