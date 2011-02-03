open SSA
open Base


type direction = Forward | Backward

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
  
    val clone_env : env -> env   
    val flow_merge : env -> ID.t -> env -> ID.t -> env -> ID.t -> env option   
    
    (* should analysis be repeated until environment stops changing? *) 
    val iterative : bool
  
    val init : fundef -> env 
  
    val value : env -> value_node -> value_info
    
    val exp_values 
      : env -> exp_node -> 
        vs:value_node list -> info:value_info list -> exp_info
    
    val exp_arr 
      : env -> exp_node -> elts:value_node list -> 
        info:value_info list -> exp_info 
        
    val exp_primapp 
      : env -> exp_node -> prim:Prim.prim -> args:value_node list ->
        argInfo:value_info list -> exp_info 
        
    val exp_call 
      : env -> exp_node -> fnId:FnId.t -> args:value_node list -> 
        info:value_info list -> exp_info 
          
    val exp_map 
      : env -> exp_node -> closure:closure -> args:value_node list -> 
        closureInfo:value_info list -> argInfo:value_info list -> exp_info 
  
    val exp_reduce 
      : env -> exp_node -> initClosure:closure -> reduceClosure:closure -> 
          args:value_node list -> initInfo : value_info list -> 
          reduceInfo:value_info list -> argInfo:value_info list -> exp_info 
    
    val exp_scan
      : env -> exp_node -> initClosure:closure -> scanClosure:closure -> 
          args:value_node list -> initInfo : value_info list -> 
          scanInfo:value_info list -> argInfo:value_info list -> exp_info 
            
    val exp_app 
      : env -> exp_node -> fn:value_node -> args:value_node list -> 
        fnInfo : value_info -> argInfo : value_info list -> exp_info 
     
    val stmt_set 
        : env -> stmt_node -> ids:ID.t list -> rhs:exp_node -> 
            rhsInfo:exp_info -> env option 
    val stmt_if 
        : env -> stmt_node -> cond:value_node -> tBlock:block -> fBlock:block ->
            gate:if_gate -> condInfo:value_info -> tEnv:env -> fEnv:env -> 
            env option    
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

module type VALUE_LATTICE = sig 
  include LATTICE
  val mk_default : value_node -> t 
end

module type EXP_LATTICE = sig 
  include LATTICE
  val mk_default : exp_node -> t 
end 

module UnitLattice  = struct
  type t = unit 
  let bottom = ()
  let combine _ _ = ()
  let eq _ _ = true
end
module ValUnit = struct
  include UnitLattice 
  let mk_default valNode = ()
end 
module ExpUnit = struct 
  include UnitLattice 
  let mk_default expNode = () 
end 

module TypeLattice  = struct
  type t = DynType.t
  let bottom = DynType.BottomT
  let combine = DynType.common_type 
  let eq = (=) 
end 
module ValType = struct 
  include TypeLattice 
  let mk_default valNode = DynType.BottomT 
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

module ExpType = struct
  include TypeListLattice   
  let mk_default expNode = List.map (fun _ -> DynType.BottomT) expNode.exp_types
end 


(* only information needed to specify a minimal imperative analysis
   which performs a no-op on every syntax node 
*)  

module MkAnalysis (S:ENV)(E:EXP_LATTICE)(V:VALUE_LATTICE)  = 
struct
  type env = S.t 
  type exp_info  = E.t 
  type value_info = V.t 
  
  let dir = Forward
  let clone_env env = env  
  let flow_merge _ _ _ _ _ _ = None   
  
  let iterative = false
  
  (* ignore the function definition and just create a fresh lattice value *)
  let init fundef = S.init fundef
  
  let value (_:env) (valNode:value_node) = V.mk_default valNode  
  
  let exp_values 
        (_:env) 
        (expNode:exp_node) 
        ~(vs:value_node list) 
        ~(info:V.t list) = E.mk_default expNode
         
  let exp_app 
        (_:env) 
        (expNode:exp_node) 
        ~(fn:value_node) 
        ~(args:value_node list) 
        ~(fnInfo:V.t)
        ~(argInfo:V.t list) = E.mk_default expNode    

  let exp_arr (_:env) (expNode:exp_node) 
        ~(elts:value_node list) ~(info:value_info list) = E.mk_default expNode 
  
  let exp_primapp 
        (_:env) 
        (expNode:exp_node) 
        ~(prim:Prim.prim) 
        ~(args:value_node list) 
        ~(argInfo:value_info list) = E.mk_default expNode 
            
  let exp_call (_:env) (expNode:exp_node) 
        ~(fnId:FnId.t) ~(args:value_node list)
        ~(info:value_info list) = E.mk_default expNode  
          
  let exp_map (_:env) (expNode:exp_node) 
        ~(closure:closure) ~(args:value_node list)
        ~(closureInfo:value_info list) ~(argInfo:value_info list) = 
          E.mk_default expNode 
        
  let exp_reduce (_:env) (expNode:exp_node) 
        ~(initClosure:closure) 
        ~(reduceClosure:closure) 
        ~(args:value_node list) 
        ~(initInfo:value_info list) 
        ~(reduceInfo:value_info list) 
        ~(argInfo:value_info list) = E.mk_default expNode  
  
  let exp_scan (_:env) (expNode:exp_node) 
        ~(initClosure:closure) 
        ~(scanClosure:closure) 
        ~(args:value_node list) 
        ~(initInfo:value_info list) 
        ~(scanInfo:value_info list)
        ~(argInfo:value_info list) = E.mk_default expNode
            
 let stmt_set 
        (_:env) 
        (_:stmt_node) 
        ~(ids:ID.t list) 
        ~(rhs:SSA.exp_node)
        ~(rhsInfo:E.t) = None 
  
  let stmt_if 
        (_:env) 
        (_:stmt_node)
        ~(cond:SSA.value_node)
        ~(tBlock:SSA.block)
        ~(fBlock:SSA.block)
        ~(gate:SSA.if_gate)
        ~(condInfo:V.t)
        ~(tEnv:env)
        ~(fEnv:env) = None 
end 

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
    | Set (ids, rhs) -> 
        A.stmt_set env stmtNode ~ids ~rhs ~rhsInfo:(eval_exp env rhs) 
    | If(cond, tBlock, fBlock, gate) -> 
        let condInfo = eval_value env cond in
        let tEnv, tChanged = eval_block (A.clone_env env) tBlock in 
        let fEnv, fChanged = eval_block (A.clone_env env) fBlock in
        let mergeChanged = ref false in 
        let folder env outId trueId falseId =
          match A.flow_merge env outId tEnv trueId fEnv falseId with
            | Some env' -> mergeChanged := true; env'  
            | None -> env
        in       
        let env' = 
          List.fold_left3 
            folder 
            env 
            gate.if_output_ids 
            gate.true_ids 
            gate.false_ids
        in begin match 
          A.stmt_if env' stmtNode 
            ~cond ~tBlock ~fBlock ~gate 
            ~condInfo ~tEnv ~fEnv 
        with 
          | None ->
            (* since None indicates no change was made we can only*)
            (* propagate it if none of the operations on child nodes*)
            (* also didn't involve changes *)
            if tChanged || fChanged || !mergeChanged 
            then Some env' else None     
         | modified -> modified
        end  
        
    | WhileLoop(condBlock, condVal, body, loopGate) -> 
        failwith "no loops yet :-("
        (*let initEnv =
          ID.Map.fold 
            (fun startId (preId,_) accEnv ->
               
            
            (A.clone_env env) 
           
        let rec loop iter =
          
          if iter > 100 then failwith "loop analysis failed to terminate"
          else  
            let preEnv = A.clone_env env in
            let startEnv = 
              ID.Map.fold 
                (fun loopStartId (preId,loopEndId) acc ->
                   A.flow_merge preEnv loopStartId env     
            let merge_inputs env out
            let startEnv =  
            let condEnv, condChanged = eval_block () condBlock in
            let condInfo = eval_value condEnv condVal in 
            let
            *)   
    | _ -> failwith ("not yet implemented: " ^ (SSA.stmt_node_to_str stmtNode))
     
  and eval_exp env expNode = match expNode.exp with 
      | App(fn, args) -> 
          A.exp_app env expNode 
            ~fn ~args 
            ~fnInfo:(eval_value env fn) ~argInfo:(eval_values env args)
      | PrimApp(prim, args) -> 
          A.exp_primapp env expNode ~prim ~args ~argInfo:(eval_values env args) 
      | Call(fnId, args) -> 
          A.exp_call env expNode ~fnId ~args ~info:(eval_values env args) 
      | Map(closure, args) ->
          A.exp_map env expNode 
            ~closure ~args 
            ~closureInfo:(eval_values env closure.closure_args) 
            ~argInfo:(eval_values env args)   
      
      | Reduce(initClosure, reduceClosure, args) -> 
          let initInfo = eval_values env initClosure.closure_args in 
          let reduceInfo = eval_values env reduceClosure.closure_args in 
          let argInfo = eval_values env args in 
          A.exp_reduce env expNode 
            ~initClosure ~reduceClosure ~args 
            ~initInfo ~reduceInfo ~argInfo
      
      | Scan(initClosure, scanClosure, args) -> 
          let initInfo = eval_values env initClosure.closure_args in 
          let scanInfo = eval_values env scanClosure.closure_args in 
          let argInfo = eval_values env args in 
          A.exp_scan env expNode
            ~initClosure ~scanClosure ~args 
            ~initInfo ~scanInfo ~argInfo
            
     | Values vs -> A.exp_values env expNode ~vs ~info:(eval_values env vs)    
     | Arr elts -> A.exp_arr env expNode ~elts ~info:(eval_values env elts)
     | _ -> failwith ("not implemented: " ^ (SSA.exp_to_str expNode))     
  and eval_value = A.value  
  and eval_values env values = List.map (eval_value env) values 

  let eval_fundef fundef = 
    let env = A.init fundef in
    let env', _ = eval_block env fundef.body in 
    env'      
end
