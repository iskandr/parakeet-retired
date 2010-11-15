open Base
open SSA

class typed_ssa_codegen =
  object (self : 'a)
     
    val types = (ref ID.Map.empty :  DynType.t ID.Map.t ref)  
    val code = (DynArray.create () : stmt_node DynArray.t)
    
    method get_type_env = !types
    method get_type id = ID.Map.find id !types 
    method add_type id t = types := ID.Map.add id t !types  
    
    method fresh_var t =
      let id = ID.gen() in 
      self#add_type id t;
      id
    
    (* returns a value node for a given variable *) 
    method id_value_node id =  
    { value_type = ID.Map.find id !types; value_src = None; value = Var id }   
    
    method cvt ~to_type ~from_type valNode = 
      if from_type = to_type then valNode 
      else ( 
        if DynType.nest_depth from_type > DynType.nest_depth  to_type then 
          failwith "Cannot convert from vector to scalar" 
        else if DynType.nest_depth to_type > DynType.nest_depth from_type then
          (* THIS IS A HACK!!! BEWARE. *)  
          (print_string"HACKHACKHACK!"; mk_stream valNode to_type) 
        else ( 
          let castNode = 
          { 
            exp = Cast(to_type, valNode);  
            exp_src = None; 
            exp_types = [to_type]
          }
          in 
          let freshId = self#fresh_var to_type in 
          let stmtNode = mk_set [freshId] castNode in  
          DynArray.add code stmtNode; 
          {value = Var freshId; value_type = to_type; value_src = None } 
       )
    )
    
    method cvt_list ~to_type ~from_types args = 
        List.map2 
            (fun  t arg -> self#cvt ~to_type ~from_type:t arg) 
            from_types 
            args 
    
    method emit stmtList = 
      List.iter (fun stmt -> DynArray.add code stmt) stmtList
    
    method finalize = DynArray.to_list code 
end

(* creates a codegen with identifiers initialized for input and output types,*)
(* passed the varnames and codegen to a user-provided function *)
(* which emits SSA into the codegen. *)
(* Once the body is finished, wrap up the code and type environment *)
(* as a fundef *)   
let mk_lambda inputTypes outputTypes fn  = 
  let codegen = new typed_ssa_codegen in 
  let inputIds = List.map codegen#fresh_var inputTypes in 
  let outputIds = List.map codegen#fresh_var outputTypes in 
  let inputVars = 
    List.map2 
        (fun id t -> {value=Var id; value_type=t; value_src=None})
        inputIds 
        inputTypes 
  in 
  let outputVars = 
    List.map2 
        (fun id t -> {value=Var id; value_type=t; value_src=None})
        outputIds 
        outputTypes
  in  
  (* allow user provided function to populate the codegen body *) 
  let _ = fn codegen inputVars outputVars in
  SSA.mk_fundef 
    ~body:codegen#finalize 
    ~tenv:codegen#get_type_env
    ~input_ids:inputIds
    ~output_ids:outputIds 