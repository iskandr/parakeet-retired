(* pp: -parser o pa_macro.cmo *)

open Base
open Imp 
open Printf 

class imp_codegen = 
  object (self)
    val types = Hashtbl.create 127
    val sharedArrayAllocations = Hashtbl.create 127
    val code : Imp.stmt DynArray.t = DynArray.create ()
    
    val inputs = (DynArray.create () : ((ID.t * DynType.t) DynArray.t))
    val inputSet : (ID.t MutableSet.t) = MutableSet.create 17
    
      
    val outputs = (DynArray.create () : ((ID.t * DynType.t) DynArray.t))
    val outputSet : (ID.t MutableSet.t) = MutableSet.create 17  
    
    val localIds : (ID.t DynArray.t) = DynArray.create ()
    val localIdSet : ( ID.t MutableSet.t) = MutableSet.create 127
    (* keep track of localIds in the order they were created *) 
     
    
    (* keep both arrays and hashtbl of sizes to maintain order of size 
       declarations but also make lookup efficient 
    *)

    val outputSizes : ((ID.t, Imp.exp_node list) Hashtbl.t) = Hashtbl.create 127
    
    val localSizes : ((ID.t, Imp.array_annot) Hashtbl.t) = Hashtbl.create 127
    
    (* prints last 10 stmts before throwing exceptions *) 
    method private fail_with_context msg = 
      let nStmts = DynArray.length code in
      Printf.eprintf "\n..."; 
      for i = max 0 (nStmts - 20) to nStmts - 1 do 
        Printf.eprintf "%d:\t %s\n" 
          (i + 1) 
          (Imp.stmt_to_str (DynArray.get code i));
      done; 
      failwith $ Printf.sprintf "ERROR IN IMP CODEGEN: %s" msg  
    
    method get_type id = Hashtbl.find types id 
    method is_shared id = Hashtbl.mem sharedArrayAllocations id 
    method shared_size id = Hashtbl.find sharedArrayAllocations id
    method is_array id = DynType.is_vec (self#get_type id)  

    method has_array_annot arrId = 
      Hashtbl.mem localSizes arrId || 
      Hashtbl.mem outputSizes arrId || 
      MutableSet.mem inputSet arrId
      
    method get_array_annot arrId =
      if MutableSet.mem localIdSet arrId  then (
        IFDEF DEBUG THEN assert (Hashtbl.mem localSizes arrId); ENDIF; 
        Hashtbl.find localSizes arrId
      ) 
      else if Hashtbl.mem outputSizes arrId then (
        IFDEF DEBUG THEN assert (Hashtbl.mem outputSizes arrId); ENDIF;   
        match Hashtbl.find outputSizes arrId with 
          | [] -> failwith "[imp_codegen] expected array" 
          | dims -> Imp.OutputArray dims   
      )
      (* array must be an input *)
      else (
        let arrT =  self#get_type arrId in
        let rank = DynType.nest_depth arrT in 
        Imp.InputArray rank 
      )
    
   
    method private add_static_size_annot id = function 
      | [] -> ()
      | sizes -> 
          IFDEF DEBUG THEN assert (MutableSet.mem localIdSet id); ENDIF; 
          Hashtbl.add localSizes id (Imp.SharedArray sizes)
                                      
    method private add_dynamic_size_annot id = function 
      | [] -> ()
      | sizes -> 
        if MutableSet.mem localIdSet id then 
          Hashtbl.add localSizes id (Imp.PrivateArray sizes) 
        else 
          Hashtbl.add outputSizes id sizes; 
   
    method add_array_annot id = function 
      | Imp.InputArray _ -> () (* these are implicitly attached to any input *)
      | Imp.PrivateArray exps -> self#add_dynamic_size_annot id exps
      | Imp.OutputArray exps -> self#add_dynamic_size_annot id exps 
      | Imp.SharedArray dims -> self#add_static_size_annot id dims 
      | Imp.InputSlice exps -> 
        Hashtbl.add localSizes id (Imp.InputSlice exps) 
        
    
    method private add_slice_annot (sliceId : ID.t) (arrId : ID.t) = 
      IFDEF DEBUG THEN 
        Printf.printf "add_slice_annot: %s = %s[]\n" (ID.to_str sliceId) (ID.to_str arrId);
      ENDIF; 
      match self#get_array_annot arrId with
        (* the array to be slice should be of at least rank 1 *) 
        | Imp.InputArray 0 
        | Imp.InputSlice []
        | Imp.OutputArray []  
        | Imp.PrivateArray [] 
        | Imp.SharedArray [] -> failwith "[imp_codegen] expected array"
        (* slice is a scalar, no new annotation needed *)
        | Imp.InputArray 1
        | Imp.InputSlice [_]
        | Imp.PrivateArray [_]
        | Imp.OutputArray [_]   
        | Imp.SharedArray [_] -> ()  
        | Imp.SharedArray dims -> 
            Hashtbl.add localSizes sliceId (Imp.SharedArray (List.tl dims))
        (* slicing either a private or output array 
           removes the outermost size expression 
        *)  
        | Imp.OutputArray expressions 
        | Imp.PrivateArray expressions -> 
          Hashtbl.add localSizes sliceId 
            (Imp.PrivateArray (List.tl expressions))
        | Imp.InputSlice expressions -> 
          Hashtbl.add localSizes sliceId 
            (Imp.InputSlice (List.tl expressions))
        | Imp.InputArray rank ->
            (*Hashtbl.add localSizes sliceId (Imp.InputArray (rank -1))*)
          
            IFDEF DEBUG THEN Printf.printf "RANK: %d\n" rank; ENDIF; 
            let arrT = self#get_type arrId in  
            let varNode = {Imp.exp=Imp.Var arrId; exp_type=arrT} in 
            let dims = List.map (fun i -> Imp.dim i varNode) (List.til rank) in 
            Hashtbl.add localSizes sliceId (Imp.InputSlice (List.tl dims))  
            
        
    (* cache the ID's into which we store BlockDim, ThreadIdx, consts, etc..*)
    val expCache  = ((Hashtbl.create 127) : (Imp.exp, ID.t) Hashtbl.t)
    
    method private add_slice_annotations = function 
      | [] -> () 
      | (Set(id, {exp=Idx({exp=Var arrId}, _)}))::rest -> 
          self#add_slice_annot id arrId; 
          self#add_slice_annotations rest 
      | _::rest -> self#add_slice_annotations rest 
    
    (* cache variables which are casts of constant expressions *) 
    val coercionCache 
        : (Imp.exp * DynType.t, Imp.exp_node) Hashtbl.t = Hashtbl.create 127
    (* create a Set node and insert a cast if necessary *)     
    method private set_or_coerce id (rhs : exp_node) =
      if not (Hashtbl.mem types id) then
        self#fail_with_context $ 
          sprintf "[imp_codegen->set_or_coerce] no type for: %s" (ID.to_str id)
      ;  
      let lhsType = self#get_type id in
      let rhsType = rhs.exp_type in 
      if lhsType <> rhsType  then (
        IFDEF DEBUG THEN
          if not $ DynType.is_scalar lhsType then 
            failwith $
              Printf.sprintf 
                "expected x%d to be a scalar, got: %s"
                id 
                (DynType.to_str lhsType)
          ; 
          if not $ DynType.is_scalar rhsType then 
            failwith $ 
              Printf.sprintf 
                "expected %s to be a scalar, got: %s"
                (Imp.exp_node_to_str rhs)
                (DynType.to_str rhsType)
          ; 
        ENDIF;
        let cacheKey = rhs.exp, lhsType in
        if Hashtbl.mem coercionCache cacheKey then
          [Set(id, Hashtbl.find coercionCache cacheKey)]
        else (
          let id' = self#fresh_local_id rhsType in
          if Imp.always_const rhs then
            Hashtbl.add coercionCache cacheKey {exp=Var id; exp_type=lhsType}
          ;
          let idExp' = {exp=Var id'; exp_type=rhsType} in
          let castNode = 
            {exp = Cast(lhsType, idExp'); exp_type=lhsType}
          in   
          [Set(id',rhs); Set(id, castNode)]
        )
      )
      (* if renaming an array, need to track its if undeclared bounds *) 
      else match rhs.exp with 
        | Var rhsId ->
          if self#is_array rhsId && not $ self#has_array_annot id then (
            IFDEF DEBUG THEN assert (self#has_array_annot rhsId); ENDIF;   
            self#add_array_annot id (self#get_array_annot rhsId);
          ); 
          [Set(id,rhs)]
        | _ -> [Set(id,rhs)] 
    
    (* an expression gets flattened into a list of statements and a simple 
       expression yielding the same value as the original compound expression 
    *) 
    method private flatten_exp expNode : Imp.stmt list * Imp.exp_node =
      IFDEF DEBUG THEN 
        Printf.printf "[ImpCodegen] Flattening exp: %s\n"
          (Imp.exp_node_to_str expNode); 
      ENDIF;
      
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
            let id = self#fresh_local_id expNode.exp_type in
            Hashtbl.add expCache exp id;     
            [Set(id, expNode)], {expNode with exp = Var id }  
           
    (* flatten the nested sub-expressions in a statement by turning them
       into their own statements 
    *) 
    method private flatten_stmt (stmt : Imp.stmt) : Imp.stmt list =
      let flatten_block block = 
        let dynArray = DynArray.create () in
        let add_stmts = List.iter (DynArray.add dynArray) in  
        List.iter (fun stmt-> add_stmts (self#flatten_stmt stmt)) block;
        DynArray.to_list dynArray 
      in 
      match stmt with 
      | If (cond, tBlock, fBlock) -> 
          let condStmts, condExp = self#flatten_exp cond in 
          let tBlock' = flatten_block tBlock in 
          let fBlock' = flatten_block fBlock in 
          condStmts @ [If(condExp, tBlock', fBlock')]
         
      | While (cond, loopBody) -> 
          let condStmts, condExp = self#flatten_exp cond in 
          let loopBody' = flatten_block loopBody in 
          condStmts @ [While(condExp, loopBody')]
      
      | Set (id, rhs) -> 
          let rhsStmts, rhs' = self#flatten_exp rhs in
          let setStmts = self#set_or_coerce id rhs' in  
          let allStmts = rhsStmts @ setStmts in 
          self#add_slice_annotations allStmts;
          allStmts   
         
     | SetIdx (id, indices, rhs) -> 
         let idxStmtLists, idxExps = 
           List.split $ List.map self#flatten_exp indices 
         in 
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
      
    method fresh_local_id  ?(dims=[]) t =
      let id = self#fresh_id t in
      MutableSet.add localIdSet id;
      DynArray.add localIds id;
      if (DynType.is_vec t && dims = []) || 
         (DynType.is_scalar t && dims <> []) then
        failwith $ Printf.sprintf 
          "[ImpCodegen] Cannot create local var of type %s and shape %s"
            (DynType.to_str t)
            (SymbolicShape.shape_to_str dims)
      ;
      if DynType.is_vec t then self#add_dynamic_size_annot id dims;
      id

    method fresh_var ?(dims = []) t =
      let id = self#fresh_local_id ~dims t in 
      {exp = Var id; exp_type = t}    
      
    method fresh_input_id t =
      let id = self#fresh_id t in
      MutableSet.add inputSet id; 
      DynArray.add inputs (id,t);
      id 
    
    method fresh_input t = {exp = Var (self#fresh_input_id t); exp_type=t} 
    
    method fresh_output_id ?(dims=[]) t = 
      let id = self#fresh_id t in
      DynArray.add outputs (id,t); 
      MutableSet.add outputSet id;
      if DynType.is_scalar t then (  
        if dims <> [] then failwith 
           (Printf.sprintf 
             "[ImpCodegen] Cannot create var of type %s and non-scalar shape %s"
             (DynType.to_str t)
             (SymbolicShape.shape_to_str dims))
        )
      else (
        if dims = [] then failwith 
          (Printf.sprintf 
             "[ImpCodegen] 
                 Cannot create var of vector type %s and scalar shape %s"
             (DynType.to_str t)
             (SymbolicShape.shape_to_str dims))
            ;   
        self#add_dynamic_size_annot id dims
      ); 
      id
      
    method fresh_output ?(dims=[]) t  = 
      let id = self#fresh_output_id ~dims t in
      {exp= Var id; exp_type=t}
    
    method private fresh_ids n t = Array.init n (fun _ -> self#fresh_local_id t)   
    
    method fresh_vars n t = 
      Array.map (fun id -> {exp=Var id;exp_type=t}) $ self#fresh_ids n t   
    
    (* TODO: reuse the Imp.exp shapes of non-shared arrays, so we can call
       the normal local_id method
    *) 
    method shared_vec_id t dims =
      let vecT = DynType.VecT t in
      let id = self#fresh_id t in
      MutableSet.add localIdSet id;
      DynArray.add localIds id;
      Hashtbl.add sharedArrayAllocations id dims; 
      self#add_static_size_annot id dims; 
      id 
      
    method shared_vec_var t dims = 
      assert (DynType.is_scalar t); 
      {exp =Var (self#shared_vec_id t dims); exp_type=DynType.VecT t} 
    
    method finalize = 
      let localIdsArr = DynArray.to_array localIds in  
      ImpSimplify.simplify_function {
        input_types =  DynArray.to_array $ DynArray.map snd inputs;
        input_ids = DynArray.to_array $ DynArray.map fst inputs; 
        
        output_types = DynArray.to_array $ DynArray.map snd outputs;   
        output_ids = DynArray.to_array $ DynArray.map fst outputs;
        output_sizes = Hashtbl.copy outputSizes; 
          
        local_ids = localIdsArr; 
        local_types = Array.map self#get_type localIdsArr;
        (* TODO: only keep PrivateArray annotations for local_arrays field of
         final function-- maybe rename this field to "private_arrays"? 
        *) 
        local_arrays =  Hashtbl.copy localSizes; 
        tenv = Hashtbl.copy types;
        shared_array_allocations = Hashtbl.copy sharedArrayAllocations;
        body = DynArray.to_list code;
      }
      
    (* first replaces every instance of the function's input/output vars *)
    (* with those provided. Then removes the input/output vars, splices the *)
    (* modified code into the target code. Returns code as a statement list *)
    method splice fn inputs outputs targetCode = 
      IFDEF DEBUG THEN 
        if (Array.length fn.input_ids  <> Array.length inputs) then 
          failwith $ Printf.sprintf 
            "Cannot splice Imp function of arity %d where %d was expected"
            (Array.length fn.input_ids)
            (Array.length inputs)
        ;
        if (Array.length fn.output_ids  <> Array.length outputs) then 
          failwith $ Printf.sprintf 
            "Cannot splice Imp function with %d output(s) where %d was expected"
            (Array.length fn.output_ids)
            (Array.length outputs)
        ;   
      ENDIF; 
      let oldIds = Array.append fn.input_ids fn.output_ids in
      (* remove the input/output ids since they will be replaced *)
      let tenv' = Hashtbl.copy fn.tenv in
      Array.iter (fun id -> Hashtbl.remove tenv' id) oldIds; 
      
      (* have to rewrite the input/output variables to the expressions *)
      (* we were given as arguments inputExps/outputVars *)
      let inOutMap = 
        Array.fold_left2 
          (fun accMap id node -> PMap.add (Var id) node.exp accMap)
          PMap.empty 
          oldIds
          (Array.append inputs outputs)
      in 
      (* generate fresh code at every splice site so that temporaries get 
         different names.  
      *)  
      let fresh_code () =  
        (* rename the local variables so they can be spliced safely into the *)
        (* targetCode *) 
        let idMap = 
          Hashtbl.fold 
            (fun id t map -> ID.Map.add id (self#fresh_id t) map)
            tenv' 
            ID.Map.empty 
        in 
        (* make sure new names of shared variables are marked as shared *)
        Hashtbl.iter  
          (fun id sz -> 
            let id' = ID.Map.find id idMap in 
            Hashtbl.add sharedArrayAllocations id' sz
          ) 
          fn.shared_array_allocations
        ;
        (* copy over all information about locals *)
        let add_local (id : ID.t) = 
          let newId = ID.Map.find id idMap in 
          DynArray.add localIds newId; 
          MutableSet.add localIdSet newId;
          if Hashtbl.mem fn.local_arrays id then 
            Hashtbl.add localSizes newId (Hashtbl.find fn.local_arrays id )  
        in 
        Array.iter add_local fn.local_ids;  
          (* TODO: Also update output size decls
    val outputSizes : ((ID.t, Imp.exp list) Hashtbl.t) = Hashtbl.create 127
           *)
      (* replace old ids with their new names in body of function *)
        let fnCode = List.map (ImpReplace.apply_id_map_to_stmt idMap) fn.body in
        List.map (ImpReplace.apply_exp_map_to_stmt inOutMap) fnCode   
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
