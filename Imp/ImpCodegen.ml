open Base
open Imp 
open Printf 

class imp_codegen = 
  object (self)
    val types = Hashtbl.create 127
    val sharedDims = Hashtbl.create 127
    val code = DynArray.create () 
    
    val inputs = (DynArray.create () : ((ID.t * DynType.t) DynArray.t)) 
    val outputs = (DynArray.create () : ((ID.t * DynType.t) DynArray.t)) 
    
    (* cache the ID's into which we store BlockDim, ThreadIdx, consts, etc..*)
    val expCache  = ((Hashtbl.create 127) : (Imp.exp, ID.t) Hashtbl.t)
    
    (* create a Set node and insert a cast if necessary *) 
     method private set_or_coerce id (rhs : exp_node) = 
      if not (Hashtbl.mem types id) then
        failwith $ 
          sprintf "[imp_codegen->set_or_coerce] no type for: %s" (ID.to_str id)
      ;  
      let lhsType = Hashtbl.find types id in
      let rhsType = rhs.exp_type in 
      if lhsType <> rhsType then 
        let id' = self#fresh_id rhsType in
        let idExp' = {exp=Var id'; exp_type=rhsType} in 
        let castNode = 
          {exp = Cast(lhsType, idExp'); exp_type=lhsType}
        in   
        [Set(id',rhs); Set(id, castNode)]
       else  [Set(id,rhs)] 
    
    (* an expression gets flattened into a list of statements and a simple 
       expression yielding the same value as the original compound expression 
    *) 
    method private flatten_exp expNode : Imp.stmt list * Imp.exp_node =
      let is_simple eNode = 
        match eNode.exp with Var _ | Const _ -> true | _ -> false 
      in   
      let flatten_arg arg = 
        if is_simple arg then  [], arg
        else
          (* flatten all arguments of the argument and collect any 
             assignments that get generated 
          *) 
          let argStmts, arg' = self#flatten_exp arg in
          (* if flattened arg is already a variable or number, just return it *)  
          if is_simple arg' then argStmts, arg' 
          else 
            (* if flattened arg is itself complex, assign it to a variable *)
            let argType = arg'.exp_type in  
            let tempId =  self#fresh_id argType in
            let setStmts = self#set_or_coerce tempId arg' in 
            let varExp = {exp = Var tempId; exp_type=argType} in 
            argStmts @  setStmts, varExp 
      in 
      let rec flatten_args ?(accStmts=[])  ?(accArgs=[]) exps =
        match exps with  
        | [] -> accStmts, List.rev accArgs
        | e::es -> 
            let stmts, e' = flatten_arg e in 
            flatten_args ~accStmts:(stmts@accStmts) ~accArgs:(e'::accArgs) es
      in   
      match expNode.exp with 
      | Const _ | Var _ -> [], expNode 
      | Cast (t1, arg) -> 
          let stmts, arg' = flatten_arg arg in 
          if arg'.exp_type = t1 then 
            stmts, arg'
          else 
            stmts, { expNode with exp =  Cast(t1, arg') }   
      | DimSize (i, arg) -> 
          let stmts, arg' = flatten_arg arg in
          let expNode' = {expNode with exp = DimSize(i,arg') } in 
           stmts, expNode'   
      | Idx (lhs, rhs) ->
          let lhsStmts, lhs' = flatten_arg lhs in 
          let rhsStmts, rhs' = flatten_arg rhs in
          lhsStmts @ rhsStmts, { expNode with exp = Idx(lhs', rhs') }   
      | Op (op, argType, args) ->
          let stmts, args' = flatten_args args in
          stmts, { expNode with exp = Op(op, argType, args') }    
      | Select (ty,cond,tExp,fExp) -> 
          (match flatten_args [cond;tExp;fExp] with 
            | stmts, [cond'; tExp'; fExp'] -> 
              stmts, { expNode with exp = Select(ty, cond', tExp', fExp') }
            | _ -> failwith "something truly bizarre happened"
          ) 
          
      | exp -> 
          if Hashtbl.mem expCache exp then 
            let id = Hashtbl.find expCache exp in 
            [], { expNode with exp = Var id }
          else
            (* don't pass a type environment since we're guaranteed to not
               be checking a variable's type  
            *) 
            let id = self#fresh_id expNode.exp_type in
            Hashtbl.add expCache exp id;     
            [Set(id, expNode)], {expNode with exp = Var id }  
           
    (* flatten the nested sub-expressions in a statement by turning them
       into their own statements 
    *) 
    method private flatten_stmt (stmt : Imp.stmt) : Imp.stmt list =
     debug $ Printf.sprintf "flattening {%d}: %s"  
       (Hashtbl.length types)  (Imp.stmt_to_str stmt)
     ;  
     match stmt with 
     | If (cond, tBlock, fBlock) -> 
         let condStmts, condExp = self#flatten_exp cond in 
         let tBlock' = List.concat $ List.map self#flatten_stmt tBlock in 
         let fBlock' = List.concat $ List.map self#flatten_stmt fBlock in 
         condStmts @ [If(condExp, tBlock', fBlock')]
         
     | While (cond, loopBody) -> 
         let condStmts, condExp = self#flatten_exp cond in 
         let loopBody' = List.concat $ List.map self#flatten_stmt loopBody in 
         condStmts @ [While(condExp, loopBody')]
     
    
     (* TODO: factor out casting code into separate function, so 
        that flatten_exp doesn't have to call flatten_stmt
     *) 
     | Set (id, rhs) -> 
        let rhsStmts, rhs' = self#flatten_exp rhs in
        let setStmts = self#set_or_coerce id rhs' in 
        rhsStmts @ setStmts 
         
     | SetIdx (id, indices, rhs) -> 
         let idxStmtLists, idxExps = 
           List.split $ List.map self#flatten_exp indices in 
         let rhsStmts, rhsExp = self#flatten_exp rhs in 
          (List.concat idxStmtLists) @ rhsStmts @ [SetIdx(id, idxExps, rhsExp)]
        
     | simple -> [simple]
     
    (* add type annotations, flatten nested expressions and then 
       add statements to the code buffer 
    *) 
    method emit block =  
      let block' = List.concat (List.map self#flatten_stmt block) in 
      List.iter (DynArray.add code) block'
    
      
    method fresh_id t =
      let id = ID.gen() in (Hashtbl.add types id t; id)
    
    method fresh_var t = {exp = Var (self#fresh_id t); exp_type = t}
      
    method fresh_input_id t = 
      let id = self#fresh_id t in 
      DynArray.add inputs (id,t);
      id 
    
    method fresh_input t = {exp = Var (self#fresh_input_id t); exp_type=t} 
    
    method fresh_output_id t = 
      let id = self#fresh_id t in
      DynArray.add outputs (id,t);
      id
     
    method fresh_output t = {exp= Var (self#fresh_output_id t); exp_type=t}
    
    method private fresh_ids n t = Array.init n (fun _ -> self#fresh_id t)   
    method fresh_vars n t = 
      Array.map (fun id -> {exp=Var id;exp_type=t}) $ self#fresh_ids n t   
    
    method shared_vec_id t dims = 
      let id = self#fresh_id (DynType.VecT t) in 
      Hashtbl.add sharedDims id dims; 
      id 
      
    method shared_vec_var t dims = 
      assert (DynType.is_scalar t); 
      {exp =Var (self#shared_vec_id t dims); exp_type=DynType.VecT t} 
     
    method find_type id = Hashtbl.find types id
    method is_shared id = Hashtbl.mem sharedDims id 
    method shared_size id = Hashtbl.find sharedDims id
    
    
    method finalize = 
     (* assert (DynArray.length inputs > 0 || DynArray.length outputs > 0);*)
      let inputIds = DynArray.to_array $ DynArray.map fst inputs in 
      let inputTypes = DynArray.to_array $ DynArray.map snd inputs in  
      let outputIds = DynArray.to_array $ DynArray.map fst outputs in 
      let outputTypes = DynArray.to_array $ DynArray.map snd outputs in
      (* use the type hashtbl to quickly filter out the id's of inputs 
         and outputs, leaving behind only local variables 
      *)
      let localTypes = Hashtbl.copy types in
      Array.iter (Hashtbl.remove localTypes) outputIds;
      Array.iter (Hashtbl.remove localTypes) inputIds;      
      let localIds = Array.of_enum $ Enum.map fst (Hashtbl.enum localTypes) in 
      let localTypes = Array.of_enum $ Enum.map snd (Hashtbl.enum localTypes) in
      {
        input_types =  inputTypes;
        input_ids = inputIds; 
        output_types = outputTypes;   
        output_ids = outputIds;  
        local_ids = localIds; 
        local_types = localTypes; 
        tenv = PMap.of_enum (Hashtbl.enum types);
        shared = PMap.of_enum (Hashtbl.enum sharedDims);
        body = DynArray.to_list code;
      }
     
    (* first replaces every instance of the function's input/output vars *)
    (* with those provided. Then removes the input/output vars, splices the *)
    (* modified code into the target code. Returns code as a statement list *)
    method splice fn inputs outputs targetCode = 
      let oldIds = Array.to_list $ Array.append fn.input_ids fn.output_ids in
      (* remove the input/output ids since they will be replaced *)
      let tenv' = PMap.remove_list oldIds fn.tenv in
      (* have to rewrite the input/output variables to the expressions *)
      (* we were given as arguments inputExps/outputVars *)
      let inOutMap = 
          List.fold_left2 
            (fun accMap id node -> PMap.add (Var id) node.exp accMap)
            PMap.empty 
            oldIds
            (Array.to_list inputs @ Array.to_list outputs)
      in 
      (* generate fresh code at every splice site so that temporaries get 
         different names.  
      *)  
      let fresh_code () =  
        (* rename the local variables so they can be spliced safely into the *)
        (* targetCode *) 
        let idMap = PMap.mapi (fun id t -> self#fresh_id t) tenv' in
        (* make sure new names of shared variables are marked as shared *)
        PMap.iter 
          (fun id sz -> Hashtbl.add sharedDims (PMap.find id idMap) sz) 
          fn.shared;
        (* replace old ids with their new names in body of function *)
          
        let fnCode = List.map (ImpCommon.apply_id_map_to_stmt idMap) fn.body in
        List.map (ImpCommon.apply_exp_map_to_stmt inOutMap) fnCode   
      in   
      let rec aux = function
        | [] -> []  
        | SPLICE::rest -> let code = fresh_code() in code @ (aux rest)
        | If (cond, tBlock, fBlock):: rest -> 
            If (cond, aux tBlock, aux fBlock) :: (aux rest) 
        | While (cond, block)::rest -> While(cond, aux block) :: (aux rest)
        | stmt::rest -> stmt :: (aux rest) 
      in
      aux targetCode 
      
   method splice_emit fn inputs outputs targetCode = 
     let code = self#splice fn inputs outputs targetCode in 
     self#emit code
         
  end 
