open Base
open Imp 

let rec find_exp_live currSet expNode = match expNode.exp with 
  | Var id -> ID.Set.add id currSet   
  | Idx (l,r) -> find_exp_live (find_exp_live  currSet l) r   
  | Op (_, _, args) -> List.fold_left find_exp_live currSet args 
  | Select (_, c,t,f) -> List.fold_left find_exp_live currSet [c;t;f] 
  | Cast (_, nested) -> find_exp_live currSet nested   
  | DimSize (_, nested) -> find_exp_live currSet nested  
  | _ -> currSet 

and find_stmt_live currSet  = function 
  | If (c,tb,fb) -> 
      find_block_live (find_block_live (find_exp_live currSet c) tb) fb   
  | While (c, body) -> find_block_live (find_exp_live currSet c) body
  | Set (_, rhs) -> find_exp_live currSet rhs  
  | SetIdx (_, indices, rhs) -> 
      List.fold_left find_exp_live currSet (rhs::indices)
  | _ -> currSet 
and find_block_live currSet = function 
  | [] -> currSet
  | stmt::rest -> find_block_live (find_stmt_live currSet stmt) rest

let rec remove_unused liveSet = function 
  | [] -> [], false  
  | stmt::rest -> 
      let rest', restChanged = remove_unused liveSet rest in 
      begin match stmt with 
        | Set(id, rhs) -> 
          if not (ID.Set.mem id liveSet) then rest', true
          else (Set (id, rhs))::rest', restChanged
        | While(cond, body) ->
            let body', bodyChanged = remove_unused liveSet body in   
          While(cond, body')::rest', restChanged || bodyChanged 
        | If(cond, tBlock, fBlock) ->
          let tBlock', tChanged = remove_unused liveSet tBlock in
          let fBlock', fChanged = remove_unused liveSet fBlock in
          let anyChanged = restChanged || tChanged || fChanged in    
          If(cond, tBlock', fBlock')::rest', anyChanged 
        | other -> other::rest', restChanged  
      end

let simple_dead_code_elim ?(maxiters=10) initLiveSet block = 
  let keepGoing = ref true in 
  let iter = ref 0 in 
  let currBlock = ref block in 
  while !iter < maxiters && !keepGoing do 
    let liveSet = find_block_live initLiveSet !currBlock in 
    let block', changed =  remove_unused liveSet !currBlock in
    keepGoing := changed; 
    currBlock := block' 
  done; 
  !currBlock   
             
let rec find_defs = function 
  | [] -> ID.Set.empty 
  | (Set(id, _))::rest -> 
      ID.Set.add id (find_defs rest)
  | (While(_, body))::rest -> 
      ID.Set.union (find_defs body) (find_defs rest)
  | (If(_, t, f))::rest -> 
      ID.Set.union (ID.Set.union (find_defs t) (find_defs f)) (find_defs rest)
  | _::rest -> find_defs rest 

let rec contains_volatile volatileSet expNode = 
  let aux = contains_volatile volatileSet in 
  match expNode.exp with 
  | Var id -> ID.Set.mem id volatileSet 
  | Idx (l,r) -> aux l || aux r    
  | Op (_,_,args) -> List.exists aux args  
  | Select (_,a,b,c)-> aux a || aux b || aux c 
  | DimSize (_, e) 
  | Cast (_, e) -> aux e   
  | _ -> false 
  
type const_env = (Imp.exp, Imp.exp_node) PMap.t 


let rec simplify_stmt 
  (constEnv : const_env) 
  (definedSet : ID.Set.t)
  (volatileSet :ID.Set.t) = function 
  | If (cond, tBlock, fBlock) -> 
      let tBlock', _, _, _  = 
          simplify_block constEnv definedSet volatileSet tBlock in 
      let fBlock', _, _, _ = 
          simplify_block constEnv definedSet volatileSet fBlock in 
      let cond' = simplify_exp constEnv volatileSet  cond in
      (* don't try to capture constness of variables like
            if (b) { x = 1} else { x = 1} 
         assume anything defined on branches in poison 
      *) 
      let tDefs = find_defs tBlock in 
      let fDefs = find_defs fBlock in 
      let volatileSet' = 
        ID.Set.union (ID.Set.union tDefs fDefs) volatileSet
      in  
      (If(cond', tBlock', fBlock')), constEnv, definedSet,  volatileSet' 
                
  | While (cond, body) ->
      (* any definitions in the body of a loop may or may not run
         so treat any definitions as potentially volatile *)
      let loopDefs = find_defs body in   
      let volatileSet' = ID.Set.union loopDefs volatileSet in
      (* remove any loop-defined variables from the constants *) 
      let constEnv = 
        ID.Set.fold (fun id env -> PMap.remove (Var id) env) 
        loopDefs 
        constEnv
      in     
      let cond' = simplify_exp constEnv volatileSet' cond in 
      let body', _, _, _ = 
        simplify_block constEnv definedSet volatileSet' body 
      in
      
      While(cond', body'),  constEnv, definedSet, volatileSet' 
  | Set (id, rhs) ->
      let definedSet, volatileSet = 
        if ID.Set.mem id definedSet then 
          ID.Set.remove id definedSet, ID.Set.add id volatileSet 
        else if ID.Set.mem id volatileSet then
          (* if it's already volatile keep things the same *)  
          definedSet, volatileSet
        else 
          (* assume variable hasn't been defined before *) 
          ID.Set.add id definedSet, volatileSet 
      in  
      (* remove all previous const declarations of this var *) 
      let constEnv = PMap.remove (Var id) constEnv in
      let nochange = (Set(id,rhs)), constEnv in    
      let stmt', constEnv' = match rhs.exp with 
        | Var id' ->  
          if PMap.mem (Var id') constEnv  
          then
            let rhs' = PMap.find (Var id') constEnv in
            (* map id to value of id' *)  
            let constEnv' = PMap.add (Var id) rhs' constEnv in
            Set(id, rhs'), constEnv'  
          else 
            (* map id->id' *) 
            Set (id, rhs), PMap.add (Var id) rhs constEnv
        | Op (op, t, args) -> 
            let args' = List.map (simplify_exp constEnv volatileSet) args in
            let rhs' = {rhs with exp = Op(op,t, args') } in  
            Set(id, rhs'), constEnv
         
        | Select (t, switch, trueNode, falseNode) -> 
            let switch' = simplify_exp constEnv volatileSet  switch in 
            let trueNode' = simplify_exp constEnv volatileSet  trueNode in 
            let falseNode' = simplify_exp constEnv volatileSet  falseNode in
            let rhs' = 
              if switch'.exp = Const (PQNum.Bool true) then trueNode'
              else if switch'.exp = Const (PQNum.Bool false) then falseNode' 
              else 
              { rhs with exp = Select(t, switch', trueNode', falseNode') }
            in    
            Set(id, rhs'), constEnv  
   
        | Cast (t, nestedNode) ->
            let nestedNode' = simplify_exp constEnv volatileSet  nestedNode in
            let rhs' = match nestedNode'.exp with 
              | Const n -> { exp = Const (PQNum.coerce_num n t); exp_type = t} 
              | _ -> {rhs with exp = Cast(t, nestedNode') } 
            in 
            Set(id, rhs'), constEnv
        | DimSize (dim, nestedNode) -> 
            let nestedNode' = simplify_exp constEnv volatileSet  nestedNode in
            let rhs' = {rhs with exp = DimSize(dim, nestedNode') } in 
            Set(id, rhs'), constEnv 
        | _ when Imp.always_const rhs -> 
            let constEnv' = PMap.add (Var id) rhs constEnv in 
            Set (id, rhs), constEnv'
        | _ -> nochange   
      
       in stmt', constEnv', definedSet, volatileSet 
      
  | SetIdx (id, indices, rhs) -> 
      let indices' = 
        List.map (simplify_exp constEnv volatileSet  ) indices in
      let rhs' = simplify_exp constEnv volatileSet  rhs in
      let volatileSet' = ID.Set.add id volatileSet in
      let constEnv' = PMap.remove (Var id) constEnv in  
      SetIdx(id, indices', rhs'), constEnv', definedSet, volatileSet' 
  | other -> other , constEnv, definedSet, volatileSet  
    
and simplify_exp (constEnv : const_env) volatileSet  expNode =
  let original = expNode.exp in 
  if PMap.mem original constEnv then 
    let cachedNode = PMap.find expNode.exp constEnv in 
    (
      if contains_volatile  volatileSet cachedNode || 
         cachedNode.exp_type <> expNode.exp_type then expNode
      else cachedNode     
    )   
  else expNode   

and simplify_block ?(revAcc=[]) constEnv definedSet volatileSet = function 
  | [] -> List.rev revAcc, constEnv, definedSet, volatileSet 
  | stmt::rest -> 
      let stmt', constEnv', definedSet', volatileSet' = 
        simplify_stmt  constEnv definedSet volatileSet stmt
      in 
      simplify_block 
        ~revAcc:(stmt'::revAcc) constEnv' definedSet volatileSet' rest  

and simplify_function fn =
  let definedSet = 
    Array.fold_left 
      (fun accSet id -> ID.Set.add id accSet) 
      ID.Set.empty 
      fn.input_ids
  in 
  let body',_,_,_ = simplify_block PMap.empty definedSet ID.Set.empty fn.body in
  let inputSet = 
    Array.fold_left (fun acc id -> ID.Set.add id acc) ID.Set.empty fn.input_ids
  in 
  let initLiveSet = 
    Array.fold_left (fun acc id -> ID.Set.add id acc) inputSet fn.output_ids 
  in 
  let body'' = simple_dead_code_elim ~maxiters:10 initLiveSet body' in 
  { fn with body = body'' }    
  