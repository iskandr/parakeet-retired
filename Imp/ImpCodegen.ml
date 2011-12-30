open Base 
open Imp 

type block_info = { 
    stmts : Imp.stmt list;
    block_ids : ID.t list;  
    block_types : ImpType.t ID.Map.t; 
    block_shapes : SymbolicShape.t ID.Map.t; 
}

class codegen  = object (self)
  val mutable ids : ID.Set.t = ID.Set.empty 
  val mutable types : ImpType.t ID.Map.t = ID.Map.empty 
  val mutable shapes : SymbolicShape.t ID.Map.t  = ID.Map.empty
  val mutable rev_body : Imp.stmt list = []
  
  method declare_local (id:ID.t) ?(shape=SymbolicShape.scalar) (ty:ImpType.t) : unit = 
    assert (ImpType.rank ty = SymbolicShape.rank shape);
    assert (not (List.mem id ids)); 
    ids <- ID.Set.add id ids; 
    types <- ID.Map.add id ty types; 
    shapes <- ID.Map.add id shape shapes
  
  method fresh_local_id ?(shape=SymbolicShape.scalar) (ty:ImpType.t) : ID.t = 
    let id = ID.gen() in
    self#declare_local id ~shape ty;
    id  
   
  method fresh_local ?(shape=SymbolicShape.scalar) (ty:ImpType.t) : value_node =
    let id = ID.gen() in   
    self#declare_local id ~shape ty;
    ImpHelpers.var ty id   
    
  method var (id:ID.t) : value_node = 
    assert (ID.Set.mem id ids); 
    let ty = ID.Map.find id types; 
    { value = Imp.Var id; value_type = ty } 
    
  method var_exp (id:ID.t) : exp_node =
    let valNode = self#var id in  
    { exp = Imp.Val valNode; exp_type = valNode.value_type }

  method set_id (id:ID.t) (rhs:exp_node) =
    rev_body <- Imp.Set(id, rhs) :: rev_body 
    
  method set (lhs:value_node) (rhs:exp_node) = 
    let id = ImpHelpers.id_of_val lhs in
    self#set_id id rhs
  
  method set_val (lhs:value_node) (rhs:value_node) = 
    self#set lhs (ImpHelpers.exp_of_val rhs)
    
  method cast (v:value_node) (ty:ImpType.t) : value_node = 
    if v.value_type = ty then v 
    else
      let temp = self#var ty in 
      (
        self#set temp (ImpHelpers.cast ty v); 
        temp
      )
  method finalize_block : block_info = 
    { 
      stmts = List.rev rev_body;
      block_ids = ID.Set.enumerate ids;
      block_types = types; 
      block_shapes = shapes; 
    }      
end

class fn_codegen = object (self)
  inherit codegen 
  val mutable input_ids : ID.t list = [] 
  val mutable input_types : ImpType.t list = [] 
  val mutable input_shapes : SymbolicShape.t list = []
  val mutable output_ids : ID.t list = []
  val mutable output_types : ImpType.t list = []
  val mutable output_shapes : SymbolicShape.t list = [] 
 
  method declare_input (id:ID.t) (t:ImpType.t) : unit = 
    input_ids <- id :: input_ids; 
    input_types <- t::input_types;
    let shape = SymbolicShape.all_dims id (ImpType.rank t) in  
    input_shapes <- shape::input_shapes
      
  method fresh_input (t:ImpType.t) : value_node =
    let id = ID.gen() in 
    self#declare_input id t;
    ImpHelpers.var t id  
  
  method declare_output (id:ID.t) ?(shape=SymbolicShape.scalar) (t:ImpType.t) : unit = 
    output_ids <- id :: input_ids; 
    output_types <- t::input_types; 
    output_shapes <- shape::input_shapes
    
  
  method fresh_output ?(shape=SymbolicShape.scalar) (t:ImpType.t) : value_node =
    let id = ID.gen() in 
    self#declare_output id ~shape t;
    ImpHelpers.var t id     
  
  method finalize_fn = 
    let blockInfo = self#finalize_block in
    let nonlocals = input_ids @ output_ids in 
    let typeEnv = 
      ID.Map.extend blockInfo.block_types nonlocals (input_types @ output_types)
    in 
    let shapeEnv = 
      ID.Map.extend blockInfo.block_shapes nonlocals (input_shapes @ output_shapes) 
    in
    {
      id = FnId.gen(); 
      input_ids = input_ids; 
      output_ids = output_ids;  
      local_ids = local_ids @ blockInfo.block_ids;  
      storage = ID.Map.empty; 
      types = typeEnv; 
      shapes = shapeEnv; 
      body = blockInfo.stmts; 
    }  
end
