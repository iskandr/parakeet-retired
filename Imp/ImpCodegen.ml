open Base
open Imp 
open Printf 

class imp_codegen = 
  object (self)
    val types = Hashtbl.create 127
    val sharedArrayAllocations = Hashtbl.create 127
    val code = DynArray.create ()
    
    val inputs = (DynArray.create () : ((ID.t * DynType.t) DynArray.t)) 
    val outputs = (DynArray.create () : ((ID.t * DynType.t) DynArray.t)) 
    
    val localIds : ( ID.t MutableSet.t) = MutableSet.create 127
    (* keep track of localIds in the order they were created *) 
    val localIdsArray : (ID.t DynArray.t) = DynArray.create () 
    
    (* keep both arrays and hashtbl of sizes to maintain order of size 
       declarations but also make lookup efficient 
    *)

    val outputSizes : ((ID.t, Imp.exp list) Hashtbl.t) = Hashtbl.create 127
    
    val localSizes : ((ID.t, Imp.array_annot) Hashtbl.t) = Hashtbl.create 127 
    
    
    method private add_static_size_annot id = function 
      | [] -> ()
      | sizes -> 
          assert (MutableSet.mem localIds id); 
          Hashtbl.add localSizes id (Imp.SharedArray sizes)
      
                
    method private add_dynamic_size_annot id = function 
      | [] -> ()
      | sizes -> 
        (* extract exp from exp_node *)
        let sizes = List.map (fun e -> e.exp) sizes in 
        if MutableSet.mem localIds id then 
          Hashtbl.add localSizes id (Imp.PrivateArray sizes) 
        else 
          Hashtbl.add outputSizes id sizes; 
    
    method private add_slice_annot (sliceId : ID.t) (arrId : ID.t) = 
      assert (MutableSet.mem localIds sliceId);
      if MutableSet.mem localIds arrId  then (
        assert (Hashtbl.mem localSizes arrId); 
        match Hashtbl.find localSizes arrId  with
        | Imp.SharedArray [] -> failwith "[imp_codegen] expected array"
        | Imp.SharedArray [_] -> () 
        | Imp.SharedArray dims -> 
          Hashtbl.add localSizes sliceId (Imp.SharedArray (List.tl dims))
        | Imp.PrivateArray [] -> failwith "[imp_codegen] expected array"
        | Imp.PrivateArray [_] -> ()
        | Imp.PrivateArray expressions -> 
          Hashtbl.add localSizes sliceId (Imp.PrivateArray (List.tl expressions))            
      ) 
      else if Hashtbl.mem outputSizes arrId then ( 
        match Hashtbl.find outputSizes arrId with 
          | [] -> failwith "[imp_codegen] expected array" 
          | [_] -> () (* nothing to do with scalar *)  
          | dims -> 
            Hashtbl.add localSizes sliceId (Imp.PrivateArray (List.tl dims))
      )
      else (* array must be an input *) (
        let arrT =  Hashtbl.find types arrId in 
        match DynType.nest_depth arrT with
          | 0 -> failwith "expected array"
          | 1 -> () (* nothing to do with scalar result *) 
          | rank -> 
              let varNode = {Imp.exp=Imp.Var arrId; exp_type=arrT} in 
              let  dims = 
                List.map (fun i -> Imp.DimSize(i, varNode)) (List.til rank)
              in 
              Hashtbl.add localSizes sliceId (Imp.PrivateArray (List.tl dims))  
      )
        
    (* cache the ID's into which we store BlockDim, ThreadIdx, consts, etc..*)
    val expCache  = ((Hashtbl.create 127) : (Imp.exp, ID.t) Hashtbl.t)
    
    method private add_slice_annotations = function 
      | [] -> () 
      | (Set(id, {exp=Idx({exp=Var arrId}, _)}))::rest -> 
          self#add_slice_annot id arrId; 
          self#add_slice_annotations rest 
      | _::rest -> self#add_slice_annotations rest 
    
    (* create a Set node and insert a cast if necessary *) 
     method private set_or_coerce id (rhs : exp_node) = 
      if not (Hashtbl.mem types id) then
        failwith $ 
          sprintf "[imp_codegen->set_or_coerce] no type for: %s" (ID.to_str id)
      ;  
      let lhsType = Hashtbl.find types id in
      let rhsType = rhs.exp_type in 
      if lhsType <> rhsType then 
        let id' = self#fresh_local_id rhsType in
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
            let tempId =  self#fresh_local_id argType in
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
          let allStmts = lhsStmts @ rhsStmts in 
          allStmts, { expNode with exp = Idx(lhs', rhs') } 
            
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
            let id = self#fresh_local_id expNode.exp_type in
            Hashtbl.add expCache exp id;     
            [Set(id, expNode)], {expNode with exp = Var id }  
           
    (* flatten the nested sub-expressions in a statement by turning them
       into their own statements 
    *) 
    method private flatten_stmt (stmt : Imp.stmt) : Imp.stmt list =
    (* debug $ Printf.sprintf "flattening {%d}: %s"  
       (Hashtbl.length types)  (Imp.stmt_to_str stmt)
     ;
    *)  
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
        let allStmts = rhsStmts @ setStmts in 
        self#add_slice_annotations allStmts;
        allStmts   
         
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
      let id = ID.gen() in 
      Hashtbl.add types id t;
      id
      
    method fresh_local_id t =
      let id = self#fresh_id t in  
      MutableSet.add localIds id;
      DynArray.add localIdsArray id;
      id

    method fresh_var t =
      let id = self#fresh_local_id t in
      {exp = Var id; exp_type = t}
     
     method fresh_array_var  t sizes =
      let id = self#fresh_local_id t in
      assert (not $ DynType.is_scalar t); 
      self#add_dynamic_size_annot id sizes;
      {exp = Var id; exp_type = t}  
      
    method fresh_input_id t =
      let id = self#fresh_id t in  
      DynArray.add inputs (id,t);
      id 
    
    method fresh_input t = {exp = Var (self#fresh_input_id t); exp_type=t} 
    
    method fresh_output_id t = 
      let id = self#fresh_id t in  
      DynArray.add outputs (id,t);
      id
      
    method fresh_array_output t sizes = 
      let id = self#fresh_output_id t in  
      self#add_dynamic_size_annot id sizes;
      {exp= Var id; exp_type=t}
    
    method fresh_output t = 
      let id = self#fresh_output_id t in
      {exp= Var id; exp_type=t}
    
    method private fresh_ids n t = Array.init n (fun _ -> self#fresh_local_id t)   
    method fresh_vars n t = 
      Array.map (fun id -> {exp=Var id;exp_type=t}) $ self#fresh_ids n t   
    
    method shared_vec_id t dims =
      let id = self#fresh_local_id (DynType.VecT t) in  
      Hashtbl.add sharedArrayAllocations id dims; 
      self#add_static_size_annot id dims; 
      id 
      
    method shared_vec_var t dims = 
      assert (DynType.is_scalar t); 
      {exp =Var (self#shared_vec_id t dims); exp_type=DynType.VecT t} 
     
    method find_type id = Hashtbl.find types id
    method is_shared id = Hashtbl.mem sharedArrayAllocations id 
    method shared_size id = Hashtbl.find sharedArrayAllocations id
    
    
    method finalize = 
     (* assert (DynArray.length inputs > 0 || DynArray.length outputs > 0);*)
      let inputIds = DynArray.to_array $ DynArray.map fst inputs in 
      let inputTypes = DynArray.to_array $ DynArray.map snd inputs in  
      let outputIds = DynArray.to_array $ DynArray.map fst outputs in 
      let outputTypes = DynArray.to_array $ DynArray.map snd outputs in
      
      let localIds = DynArray.to_array localIdsArray in 
      let localTypes = Array.map (fun id -> Hashtbl.find types id) localIds in
          
      ImpSimplify.simplify_function {
        input_types =  inputTypes;
        input_ids = inputIds; 
        
        output_types = outputTypes;   
        output_ids = outputIds;
        output_sizes = PMap.of_enum (Hashtbl.enum outputSizes); 
          
        local_ids = localIds; 
        local_types = localTypes;
        local_arrays = PMap.of_enum (Hashtbl.enum localSizes);
           
        tenv = PMap.of_enum (Hashtbl.enum types);
        shared_array_allocations = PMap.of_enum (Hashtbl.enum sharedArrayAllocations);
        
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
          (fun id sz -> 
              Hashtbl.add sharedArrayAllocations (PMap.find id idMap) sz) 
          fn.shared_array_allocations;
        (* copy over all information about locals *)
        let add_local (id : ID.t) = 
          let (newId : ID.t) = PMap.find id idMap in 
          DynArray.add localIdsArray newId; 
          MutableSet.add localIds newId;
          if PMap.mem id fn.local_arrays then 
            Hashtbl.add localSizes newId (PMap.find id fn.local_arrays)  
        in 
        Array.iter add_local fn.local_ids;  
          (* TODO: Also update output size decls
    val outputSizes : ((ID.t, Imp.exp list) Hashtbl.t) = Hashtbl.create 127
           *)
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
