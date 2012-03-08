(* pp: -parser o pa_macro.cmo *)

open Base
open Imp

type fn_info = {
  mutable ids : ID.Set.t;
  mutable types : ImpType.t ID.Map.t;
  mutable shapes : SymbolicShape.t ID.Map.t;
  mutable storages : Imp.storage ID.Map.t;
}


let create_fn_info () =
  {
    ids = ID.Set.empty;
    types = ID.Map.empty;
    shapes = ID.Map.empty;
    storages = ID.Map.empty
  }


let id_of valNode = match valNode.value with
  | Var id ->  id
  | other -> failwith $ "Can't set " ^ (Imp.value_to_str other)

let rec remove_pos_from_list  ?(curr=0) pos = function
  | [] -> []
  | x::xs ->
    if curr = pos then xs
    else x :: (remove_pos_from_list ~curr:(curr+1) pos xs)


class builder (info:fn_info) = object (self)
  val stmts : stmt DynArray.t = DynArray.create ()
  method info = info
  method clone = new builder info
  method to_list = DynArray.to_list stmts
  method get_type id = ID.Map.find id info.types
  method get_shape id = ID.Map.find id info.shapes
  method get_storage id = ID.Map.find id info.storages
  method has_id (id:ID.t) = ID.Set.mem id info.ids


  (* TODO: actually implement this *)
  method flatten stmt = [stmt]

  method prepend stmt =
    let rec aux i = function
      | [] -> ()
      | x::xs ->
        DynArray.insert stmts i x;
        aux (i+1) xs
    in
    aux 0 (self#flatten stmt)


  method append stmt : unit =
    List.iter (DynArray.add stmts) (self#flatten stmt)

  method concat_list stmts = List.iter self#append stmts
  method concat (other:builder) = self#concat_list other#to_list

  method declare (id:ID.t) ?storage ?shape (ty:ImpType.t) =
    let storage = match storage with
      | Some storage -> storage
      | None ->
        if ImpType.is_scalar ty then Imp.Local
        else failwith $ Printf.sprintf
          "Variable %s : %s cannot be declared without storage"
          (ID.to_str id)
          (ImpType.to_str ty)
    in
    let shape = match shape with
      | Some shape -> shape
      | None ->
        if ImpType.is_scalar ty then SymbolicShape.scalar
        else failwith $ Printf.sprintf
          "Variable %s : %s cannot be declared without shape"
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
    ENDIF;
    IFDEF DEBUG THEN
      if ID.Set.mem id info.ids then
        failwith $
          Printf.sprintf
            "[ImpBuilder] Identifier already declared: %s"
            (ID.to_str id)
    ENDIF;
    info.ids <- ID.Set.add id info.ids;
    info.types <- ID.Map.add id ty info.types;
    info.shapes <- ID.Map.add id shape info.shapes;
    info.storages <- ID.Map.add id storage info.storages

  method fresh_local name ?storage ?shape (ty:ImpType.t)  : value_node =
    let id = ID.gen_named name in
    self#declare  id ?storage ?shape ty;
    { value = Var id; Imp.value_type = ty }

  method cast name (v:value_node) (ty:ImpType.t)  =
    if v.value_type <> ty then v
    else (
      let temp : Imp.value_node = self#fresh_local name ty in
      self#append $Set(temp, {Imp.value_type = ty; value = Imp.Cast(ty, v)});
      temp
    )

  method var (id:ID.t)  : value_node =
    if not $ self#has_id id
    then failwith $ "[ImpBuilder] ID not found: " ^ ID.to_str id
    else
    let ty = self#get_type id in
    { value = Imp.Var id; value_type = ty }

  method value_shape {value} = match value with
    | Var id -> self#get_shape id
    | FixDim(arr, dim, idx) ->
      let arrShape = self#value_shape arr in
      remove_pos_from_list (ImpHelpers.get_const_int dim) arrShape
    | VecConst ns -> [SymbolicShape.Const (List.length ns)]
    | VecSlice (_, _, nelts) -> [SymbolicShape.Const nelts]
    | _ -> SymbolicShape.scalar

  (* recursively build fixdim nodes for a list of indices *)
  method fixdims ~arr ~dims ~indices  : value_node =
    match dims, indices with
      | [], [] -> arr
      | [_], [i] when ImpType.rank arr.value_type = 1 -> ImpHelpers.idx arr [i]
      | d::ds, i::is ->
        let rhs = ImpHelpers.fixdim arr d i in
        let rhsShape = self#value_shape rhs in
        let rhsType = ImpType.peel arr.value_type in
        let lhs =
          self#fresh_local  "fixdim" ~storage:Imp.Alias ~shape:rhsShape rhsType
        in
        self#append $ Imp.Set(lhs, rhs);
        self#fixdims ~arr:lhs ~dims:ds ~indices:is
      | _ -> failwith "Expected dims and indices to be of same length"

  method idx_or_fixdims
    ~(arr:value_node)
    ~(dims:value_nodes)
    ~(indices:value_nodes) =
    let nIndices = List.length indices in
    let arrT = arr.value_type in
    let rank = ImpType.rank arrT in
    IFDEF DEBUG THEN
      let nDims = List.length dims in
      if nDims <> nIndices then
        failwith $ Printf.sprintf
          "[idx_or_fixdims] # of dims (%d) =/=  # of indices (%d)"
          nDims
          nIndices
      ;
      if (nIndices > rank) && (rank > 0) then
        failwith $ Printf.sprintf
          "[idx_or_fixdims] Can't index into rank %d array with %d indices"
          rank
          nIndices
      ;
    ENDIF;
    (* for convenience, treat indexing into scalars as the identity operation *)
    if rank = 0 then  arr
    else if rank = nIndices && (List.for_all ImpHelpers.is_const_int dims) then
      ImpHelpers.idx arr ~dims indices
    else self#fixdims arr dims indices

  method build_add  (name:string) (x:value_node) (y:value_node) : value_node =
    assert false

  method inline (impFn:Imp.fn) (inputs:value_nodes) (outputs:value_nodes) =
    let rename_local oldId =
      let name = ID.get_original_prefix oldId in
      let t = ID.Map.find oldId impFn.Imp.types in
      let shape = ID.Map.find oldId impFn.Imp.shapes in
      let storage = ID.Map.find oldId impFn.Imp.storage in
      self#fresh_local name ~storage ~shape t
    in
    let newLocalVars = List.map rename_local impFn.local_ids in
    let replaceEnv =
      ID.Map.of_lists
        (impFn.input_ids @ impFn.output_ids @ impFn.local_ids)
        (inputs @ outputs @ newLocalVars)
    in
    let newBody = ImpReplace.replace_block replaceEnv impFn.body in
    self#concat_list newBody
end


let (+=) builder stmt = builder#append stmt


class fn_builder (name:string) = object (self)
  val fn_id = FnId.gen_named name
  val info = create_fn_info()

  (* block builders take fn_builders as a param so they can keep track of *)
  (* type, shape, storage info *)
  val body = ref None

  method body = match !body with
    | None ->
      let builder = new builder info in
      body := Some builder;
      builder
    | Some builder -> builder

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
    let storage = if ImpType.is_scalar t then Imp.Local else Imp.Alias in
    self#body#declare id ~storage ~shape t

  method fresh_input (t:ImpType.t) : value_node =
    let id = ID.gen_named "input" in
    self#declare_input id t;
    ImpHelpers.var t id

  method declare_output (id:ID.t) ?(shape=SymbolicShape.scalar) (t:ImpType.t) =
    output_ids <- output_ids @ [id];
    output_types <- output_types @ [t];
    output_shapes <- output_shapes @ [shape];
    let storage = if ImpType.is_scalar t then Imp.Local else Imp.Global in
    self#body#declare id ~storage ~shape t

  method fresh_output ?(shape=SymbolicShape.scalar) (t:ImpType.t) : value_node =
    let id = ID.gen_named "output" in
    self#declare_output id ~shape t;
    ImpHelpers.var t id


  method finalize_fn =
    let nonlocals = input_ids @ output_ids in
    let localIds =
      List.filter
        (fun id -> not $ List.mem id nonlocals)
        (ID.Set.elements info.ids)
    in
    {
      Imp.id = fn_id;
      input_ids = input_ids;
      output_ids = output_ids;
      local_ids = localIds;
      storage = info.storages;
      types = info.types;
      shapes = info.shapes;
      body = self#body#to_list;
    }
end
