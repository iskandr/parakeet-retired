(* pp: -parser o pa_macro.cmo *)

open Base
open Imp 
open Printf 


(* Classes in this modue: 

   code_block = 
    container for scope-specific code which will be turned into an Imp block 

   fn_state = 
     variable info for whole function, which may contain many blocks
   
   There is also a class type called fn_state_interface which is necessary 
   only because OCaml doesn't let self variables escape a class, 
   so we have to explicitly construct a class type for 
   fn_state which it can pass to code_block
*) 

class type fn_state_interface = object 
  method has_type : ID.t -> bool 
  method get_type : ID.t -> DynType.t
  
  method get_id_shape : ID.t -> SymbolicShape.shape
  method get_exp_shape : Imp.exp_node -> SymbolicShape.shape
  method set_shape : ID.t -> SymbolicShape.shape -> unit
       
  method is_shared : ID.t -> bool  
  method is_array : ID.t -> bool   

  method has_array_storage : ID.t -> bool 
  method get_array_storage : ID.t -> Imp.array_storage
  method set_array_storage : ID.t -> Imp.array_storage -> unit
   
  method fresh_id : DynType.t -> ID.t 
  
  method fresh_local_id : 
      ?dims:(Imp.exp_node list) -> ?storage:Imp.array_storage -> 
        DynType.t -> ID.t
         
  method add_to_locals : ID.t -> unit 
  
  method fresh_var : 
      ?dims:(Imp.exp_node list) -> ?storage:Imp.array_storage -> 
        DynType.t -> Imp.exp_node
  method fresh_vars : int -> DynType.t -> Imp.exp_node array
    
  method fresh_input_id : DynType.t -> ID.t 
  method fresh_input : DynType.t -> Imp.exp_node 
    
  method fresh_output_id : ?dims:(Imp.exp_node list) -> DynType.t -> ID.t 
  method fresh_output : ?dims:(Imp.exp_node list) -> DynType.t -> Imp.exp_node 
    
  method shared_vec_id : DynType.t -> int list -> ID.t 
  method shared_vec_var : DynType.t -> int list -> Imp.exp_node
end

let is_simple eNode = match eNode.exp with   
  | Var _ 
  | Const _ -> true 
  | _ -> false 



class code_buffer (fnState: fn_state_interface) = object(self)
  val code : Imp.stmt DynArray.t = DynArray.create ()
  
  method to_block = DynArray.to_list code 
  (* add type annotations, flatten nested expressions and then 
       add statements to the code buffer 
    *) 
  method emit block =  
    let block' = List.concat (List.map self#flatten_stmt block) in 
    List.iter (DynArray.add code) block'
  
  (* cache the ID's into which we store BlockDim, ThreadIdx, consts, etc..*)
  val expCache  = ((Hashtbl.create 127) : (Imp.exp, ID.t) Hashtbl.t)
    
  (* cache variables which are casts of constant expressions *) 
  val coercionCache : (Imp.exp * DynType.t, Imp.exp_node) Hashtbl.t = 
    Hashtbl.create 127
  (* prints last 10 stmts before throwing exceptions *) 
  method private fail_with_context msg = 
    let nStmts = DynArray.length code in
    Printf.printf "\n..."; 
    for i = max 0 (nStmts - 20) to nStmts - 1 do 
    Printf.printf "%d:\t %s\n" 
      (i + 1) 
      (Imp.stmt_to_str (DynArray.get code i));
    done; 
    failwith $ Printf.sprintf "ERROR IN IMP CODEGEN: %s" msg
 
    
  method private register_slice_storage = function 
    | [] -> () 
    | (Set(id, {exp=Idx({exp=Var arrId}, _)}))::rest 
      when DynType.nest_depth (fnState#get_type arrId) > 1 ->
        fnState#set_array_storage id Imp.Slice;  
        self#register_slice_storage rest 
    | _::rest -> self#register_slice_storage rest 
           
 (* create a Set node and insert a cast if necessary *)     
  method private set_or_coerce id (rhs : exp_node) =
    if not (fnState#has_type id) then
      self#fail_with_context $ 
         sprintf "[imp_codegen->set_or_coerce] no type for: %s" (ID.to_str id);  
    let lhsType = fnState#get_type id in
    let rhsType = rhs.exp_type in 
    if lhsType <> rhsType  then (
      IFDEF DEBUG THEN
      if not $ DynType.is_scalar lhsType then 
        failwith $
          Printf.sprintf 
            "expected x%d to be a scalar %s, got: %s"
            id 
            (DynType.to_str rhsType)
            (DynType.to_str lhsType)
        ; 
      if not $ DynType.is_scalar rhsType then 
        failwith $ 
          Printf.sprintf 
            "expected %s to be a scalar %s, got: %s"
            (Imp.exp_node_to_str rhs)
            (DynType.to_str lhsType)
            (DynType.to_str rhsType)
          ; 
        ENDIF;
      let cacheKey = rhs.exp, lhsType in
        if Hashtbl.mem coercionCache cacheKey then
          [Set(id, Hashtbl.find coercionCache cacheKey)]
        else (
          let shape = fnState#get_exp_shape rhs in 
          let id' = 
            fnState#fresh_local_id ~dims:shape ~storage:Imp.Slice rhsType  
          in
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
      else match rhs.exp with  (*
        | Var rhsId ->
          if fnState#is_array rhsId && not $ fnState#has_array_annot id then (
            IFDEF DEBUG THEN assert (fnState#has_array_annot rhsId); ENDIF;   
            fnState#add_array_annot id (fnState#get_array_annot rhsId);
          ); 
          [Set(id,rhs)]  *)
        | _ -> [Set(id,rhs)] 
    
    
    
    (* an expression gets flattened into a list of statements and a simple 
       expression yielding the same value as the original compound expression 
    *) 
    method private flatten_exp expNode : Imp.stmt list * Imp.exp_node =
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
            let argShape = fnState#get_exp_shape arg' in 
            let tempId : ID.t = 
              fnState#fresh_local_id ~dims:argShape ~storage:Imp.Slice argType 
            in
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
            let shape = fnState#get_exp_shape expNode in
            let t = expNode.exp_type in  
            let id = fnState#fresh_local_id ~dims:shape ~storage:Imp.Slice t  in
            Hashtbl.add expCache exp id;     
            [Set(id, expNode)], {expNode with exp = Var id }  


(* 
  flatten the nested sub-expressions in a statement by turning them
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
        self#register_slice_storage allStmts;
        allStmts   
         
     | SetIdx (id, indices, rhs) -> 
         let idxStmtLists, idxExps = 
           List.split $ List.map self#flatten_exp indices 
         in 
         let rhsStmts, rhsExp = self#flatten_exp rhs in 
         (List.concat idxStmtLists) @ rhsStmts @ [SetIdx(id, idxExps, rhsExp)]
        
     | simple -> [simple]

(* first replaces every instance of the function's input/output vars *)
(* with those provided. Then removes the input/output vars, splices the *)
(* modified code into the target code. Returns code as a statement list *)
  method splice fn inputs outputs targetCode = 
    IFDEF DEBUG THEN 
      if (Array.length fn.input_ids  <> Array.length inputs) then 
        failwith $ Printf.sprintf 
          "Cannot splice Imp function of arity %d where %d was given"
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
       
    (* have to rewrite the input/output variables to the expressions *)
    (* we were given as arguments inputExps/outputVars *)
    let inOutMap : Imp.exp_node ID.Map.t = 
      Array.fold_left2 
        (fun accMap id node -> ID.Map.add id node accMap)
        ID.Map.empty 
        (Array.append fn.input_ids fn.output_ids)
        (Array.append inputs outputs)
    in
    (* build an ID->ID map, and register types and storage of inlined 
         vars. Don't yet add size info, since we need to translate
         variable names in the size expressions 
      *) 
    let build_id_map id t map =
      if MutableSet.mem fn.local_id_set id then (
        let id' = fnState#fresh_id t in
        fnState#add_to_locals id'; 
        if not (DynType.is_scalar t) then (
          assert (Hashtbl.mem fn.array_storage id);
          fnState#set_array_storage id' (Hashtbl.find fn.array_storage id); 
          let oldShape = Hashtbl.find fn.sizes id in
          let shape = 
            List.map (ImpReplace.apply_exp_map inOutMap) oldShape 
          in
          fnState#set_shape id' shape;    
        ); 
        ID.Map.add id id' map
      )
      else map
    in  
    (* generate fresh code at every splice site so that temps get new names. *)  
    let fresh_code () =  
      let idMap = Hashtbl.fold build_id_map fn.types ID.Map.empty in           
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

and fn_state = object (self)
    val types : (ID.t, DynType.t) Hashtbl.t = Hashtbl.create 127
    val array_storage : (ID.t, Imp.array_storage) Hashtbl.t = Hashtbl.create 127
    val sizes : (ID.t, Imp.exp_node list) Hashtbl.t = Hashtbl.create 127 
    
    val inputs : ID.t DynArray.t = DynArray.create () 
    val inputSet : (ID.t MutableSet.t) = MutableSet.create 17
    
    val outputs : ID.t DynArray.t =  DynArray.create ()
    val outputSet : (ID.t MutableSet.t) = MutableSet.create 17  
    
    val localIdSet : ID.t MutableSet.t = MutableSet.create 127
    
  
    method fresh_code_buffer = new code_buffer (self :> fn_state_interface)
    (* not allowed to create instance variables that pass self around, 
       since self is not yet done being constructed
    *) 
    val mutable mainCodeBuffer = None 
    
    method main_code_buffer = match mainCodeBuffer with 
      | None -> 
        let b = self#fresh_code_buffer in 
        mainCodeBuffer <- Some b; 
        b
      | Some b -> b  
    
    method has_type id = Hashtbl.mem types id 
    method get_type id = Hashtbl.find types id 
    
    method get_id_shape id = Hashtbl.find sizes id 
    method get_exp_shape expNode = match expNode.exp with 
      | Imp.Var id -> self#get_id_shape id 
      | Imp.Idx (array, idx) -> 
          let fullShape = self#get_exp_shape array in 
          SymbolicShape.peel_shape fullShape 
      | _ -> SymbolicShape.scalar
    method set_shape id shape = Hashtbl.replace sizes id shape
    
    method is_shared id = 
      Hashtbl.mem array_storage id && Hashtbl.find array_storage id = Imp.Shared
         
    method is_array id = DynType.is_vec (self#get_type id)  
      
    method has_array_storage arrId = Hashtbl.mem array_storage arrId   
    method get_array_storage arrId = Hashtbl.find array_storage arrId 
    method set_array_storage arrId = Hashtbl.replace array_storage arrId 


    method fresh_id t = 
      let id = ID.gen() in 
      Hashtbl.add types id t;
      id
      
    method add_to_locals id = MutableSet.add localIdSet id
    
    method fresh_local_id  ?(dims=[]) ?(storage=Imp.Slice) t =
      let id = self#fresh_id t in
      self#add_to_locals id; 
      self#set_shape id dims;
      if DynType.is_vec t then (
        match dims with 
          | [] ->    
            failwith $ Printf.sprintf 
              "[ImpCodegen] Local var of type %s can't have scalar shape"
              (DynType.to_str t)
          | _ ->

            self#set_array_storage id storage 
      );  
      id

    method fresh_var ?(dims = []) ?(storage=Imp.Slice) t =
      let id = self#fresh_local_id ~dims ~storage t in 
      {exp = Var id; exp_type = t}    
      
    
    method fresh_input_id t =
      let id = self#fresh_id t in
      Hashtbl.add sizes id (SymbolicShape.all_dims (Imp.var ~t id));  
      MutableSet.add inputSet id;
      if DynType.is_vec t then 
        Hashtbl.add array_storage id Imp.Global
      ; 
      DynArray.add inputs id;    
      id 
    
    method fresh_input t = {exp = Var (self#fresh_input_id t); exp_type=t} 
    
    method fresh_output_id ?(dims=[]) t = 
      let id = self#fresh_id t in
      Hashtbl.add sizes id dims; 
      DynArray.add outputs id;  
      MutableSet.add outputSet id;
     
      if DynType.is_scalar t then (  
        if dims <> [] then failwith $ 
           Printf.sprintf 
             "[ImpCodegen] Cannot create var of type %s and non-scalar shape %s"
             (DynType.to_str t)
             (SymbolicShape.shape_to_str dims)
      )
      else (
        Hashtbl.add array_storage id Imp.Global; 
        if dims = [] then failwith $ 
          Printf.sprintf 
             "[ImpCodegen] 
                 Cannot create var of vector type %s and scalar shape %s"
             (DynType.to_str t)
             (SymbolicShape.shape_to_str dims);   
      ); 
      id
      
    method fresh_output ?(dims=[]) t  = 
      let id = self#fresh_output_id ~dims t in
      {exp= Var id; exp_type=t}
    
    method private fresh_ids n t = Array.init n (fun _ -> self#fresh_local_id t)   
    
    method fresh_vars n t = 
      Array.map (fun id -> {exp=Var id;exp_type=t}) $ self#fresh_ids n t   
     
    method shared_vec_id t intDims =
      let id = self#fresh_id t in
      Hashtbl.add sizes id (List.map Imp.int intDims); 
      MutableSet.add localIdSet id;
      Hashtbl.add array_storage id Imp.Shared; 
      id 
      
    method shared_vec_var t dims = 
      assert (DynType.nest_depth t = 1); 
      {exp =Var (self#shared_vec_id t dims); exp_type= t} 
    
    method finalize = 
      let inputArray = DynArray.to_array inputs in 
      let outputArray = DynArray.to_array outputs in 
      let impFn = ImpSimplify.simplify_function {
        input_ids = inputArray;
        input_id_set = inputSet; 
        input_types = Array.map self#get_type inputArray; 
   
        output_ids = outputArray;
        output_id_set = outputSet;  
        output_types = Array.map self#get_type outputArray;    
        
        local_id_set = localIdSet; 

        types = types;
        sizes =  sizes; 
        
        array_storage = array_storage;

        body = self#main_code_buffer#to_block;
      }
      in 
      IFDEF DEBUG THEN 
        Printf.printf "Finalizing Imp function: %s\n " (Imp.fn_to_str impFn); 
      ENDIF; 
      impFn  
  end 
