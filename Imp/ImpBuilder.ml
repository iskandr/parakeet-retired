(* pp: -parser o pa_macro.cmo *)

open Base
open Imp

type block_info = {
    block_ids : ID.t list;
    block_types : ImpType.t ID.Map.t;
    block_shapes : SymbolicShape.t ID.Map.t;
    block_storages : Imp.storage ID.Map.t;
}

class builder = object (self)
  val mutable ids : ID.Set.t = ID.Set.empty
  val mutable types : ImpType.t ID.Map.t = ID.Map.empty
  val mutable shapes : SymbolicShape.t ID.Map.t = ID.Map.empty
  val mutable storages : Imp.storage ID.Map.t = ID.Map.empty

  method declare (id:ID.t) ?storage ?shape (ty:ImpType.t) =
    let storage = match storage with
      | Some storage -> storage
      | None ->
        if ImpType.is_scalar ty then Imp.Stack
        else failwith $ Printf.sprintf
          "[ImpBuilder] Variable %s : %s cannot be declared without storage"
          (ID.to_str id)
          (ImpType.to_str ty)
    in
    let shape = match shape with
      | Some shape -> shape
      | None ->
        if ImpType.is_scalar ty then SymbolicShape.scalar
        else failwith $ Printf.sprintf
          "[ImpBuilder] Variable %s : %s cannot be declared without shape"
          (ID.to_str id)
          (ImpType.to_str ty)
    in
    IFDEF DEBUG THEN
      if ImpType.rank ty <> SymbolicShape.rank shape then
        failwith $
          Printf.sprintf
            "[ImpBuilder] Mismatch between rank of type %s and shape %s for %s"
              (ImpType.to_str ty)
              (SymbolicShape.to_str shape)
              (ID.to_str id)
     ;
      if not (ID.Set.mem id ids) then
        failwith $
          Printf.sprintf "[ImpBuilder] Undeclared identifier %s" (ID.to_str id)
    ENDIF;
    ids <- ID.Set.add id ids;
    types <- ID.Map.add id ty types;
    shapes <- ID.Map.add id shape shapes;
    storages <- ID.Map.add id storage storages

  method fresh_local_id ?name ?storage ?shape(ty:ImpType.t) : ID.t =
    let id = match name with
      | None -> ID.gen()
      | Some name -> ID.gen_named name
    in
    self#declare id ?storage ?shape ty;
    id

  method fresh_local ?name ?storage ?shape (ty:ImpType.t) : value_node =
    let id = match name with
      | None -> ID.gen()
      | Some name -> ID.gen_named name
    in
    self#declare id ?storage ?shape ty;
    ImpHelpers.var ty id

  method var (id:ID.t) : value_node =
    if not $ ID.Set.mem id ids
    then failwith $ "[ImpCodegen] ID not found: " ^ ID.to_str id
    else
    let ty = ID.Map.find id types in
    { value = Imp.Var id; value_type = ty }

  method cast (v:value_node) (ty:ImpType.t) : value_node * stmt list =
    if v.value_type = ty then v, []
    else
      let name = "%cast_" ^ (ImpType.to_str ty) in
      let temp : Imp.value_node = self#fresh_local ~name ty in
      temp, [ImpHelpers.set temp (ImpHelpers.cast ty v)]


  method get_shape id = ID.Map.find id shapes
  method get_type id = ID.Map.find id types
  method get_storage id = ID.Map.find id storages

  method info : block_info =
    {
      block_ids = ID.Set.to_list ids;
      block_types = types;
      block_shapes = shapes;
      block_storages = storages;
    }
end

class fn_builder = object (self)
  inherit builder
  val mutable input_ids : ID.t list = []
  val mutable input_types : ImpType.t list = []
  val mutable input_shapes : SymbolicShape.t list = []

  val mutable output_ids : ID.t list = []
  val mutable output_types : ImpType.t list = []
  val mutable output_shapes : SymbolicShape.t list = []

  method declare_input (id:ID.t) (t:ImpType.t) : unit =
    input_ids <- input_ids @ [id];
    input_types <- input_types @ [t];
    let shape = SymbolicShape.all_dims id (ImpType.rank t) in
    input_shapes <- input_shapes @ [shape];
    let storage = if ImpType.is_scalar t then Imp.Stack else Imp.Alias in
    self#declare id ~storage ~shape t

  method fresh_input (t:ImpType.t) : value_node =
    let id = ID.gen_named "input" in
    self#declare_input id t;
    ImpHelpers.var t id

  method declare_output (id:ID.t) ?(shape=SymbolicShape.scalar) (t:ImpType.t) =
    output_ids <- output_ids @ [id];
    output_types <- output_types @ [t];
    output_shapes <- output_shapes @ [shape];
    let storage = if ImpType.is_scalar t then Imp.Stack else Imp.HeapAlloc in
    self#declare id ~storage ~shape t

  method fresh_output ?(shape=SymbolicShape.scalar) (t:ImpType.t) : value_node =
    let id = ID.gen_named "output" in
    self#declare_output id ~shape t;
    ImpHelpers.var t id

  method finalize_fn ?name stmts =
    let blockInfo = self#info in
    let nonlocals = input_ids @ output_ids in
    let localIds =
      List.filter (fun id -> not $ List.mem id nonlocals) blockInfo.block_ids
    in
    let id = match name with
      | Some name -> FnId.gen_named name
      | None -> FnId.gen()
    in
    {
      id = id;
      input_ids = input_ids;
      output_ids = output_ids;
      local_ids = localIds;
      storage = blockInfo.block_storages;
      types = blockInfo.block_types;
      shapes = blockInfo.block_shapes;
      body = stmts;
    }
end
