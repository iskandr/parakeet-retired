open Imp 

type block_info = { 
    stmts : Imp.stmt list;
    block_ids : ID.t list;  
    block_types : ImpType.t ID.Map.t; 
    block_shapes : SymbolicShape.t ID.Map.t; 
}

class codegen  = object (self)
  val mutable ids : ID.t list = [] 
  val mutable types : ImpType.t ID.Map.t = ID.Map.empty 
  val mutable shapes : SymbolicShape.t ID.Map.t  = ID.Map.empty
  val mutable rev_body : Imp.stmt list = []
  
  method fresh_id ?(shape=SymbolicShape.scalar) (ty:ImpType.t) : ID.t = 
    assert (ImpType.rank ty = SymbolicShape.rank shape); 
    let id = ID.gen() in
    ids <- id :: ids; 
    types <- ID.Map.add id ty types; 
    shapes <- ID.Map.add id shape shapes;
    id  
  
  method var ?(shape=SymbolicShape.scalar) (ty:ImpType.t) : value_node =
    let id = self#fresh_id ~shape ty in  
    ImpHelpers.var ty id
    
  method var_exp ?(shape=SymbolicShape.scalar) ty = 
    ImpHelpers.exp_of_val (self#var ~shape ty)

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
      block_ids = ids;
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
 
  method named_input (id:ID.t) (t:ImpType.t) : value_node = 
    input_ids <- id :: input_ids; 
    input_types <- t::input_types;
    let shape = SymbolicShape.all_dims id (ImpType.rank t) in  
    input_shapes <- shape::input_shapes; 
    ImpHelpers.var t id 
      
  method fresh_input (t:ImpType.t) : value_node =
    let id = ID.gen() in 
    self#named_input id ~shape t 
  
  method named_output (id:ID.t) ?(shape=SymbolicShape.scalar) (t:ImpType.t) : value_node = 
    output_ids <- id :: input_ids; 
    output_types <- t::input_types; 
    output_shapes <- shape::input_shapes; 
    ImpHelpers.var t id 
  
  method fresh_output ?(shape=SymbolicShape.scalar) (t:ImpType.t) : value_node =
    let id = ID.gen() in 
    self#named_output id ~shape t   
    
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
      local_ids = blockInfo.block_ids;  
      storage = ID.Map.empty; 
      types = typeEnv; 
      shapes = shapeEnv; 
      body = blockInfo.stmts; 
    }  
end
