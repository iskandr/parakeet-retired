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

let rec remove_pos_from_list ?(curr=0) pos = function
  | [] -> []
  | x::xs ->
    if curr = pos then xs
    else x :: (remove_pos_from_list ~curr:(curr+1) pos xs)

let is_simple {value} = match value with
  | Var _
  | Const _
  | VecConst _
  | CudaInfo _ -> true
  | _ -> false

class builder (info:fn_info) = object (self)
  val stmts : stmt DynArray.t = DynArray.create ()
  method info = info
  method clone = new builder info
  method to_list = DynArray.to_list stmts
  method get_type id = ID.Map.find id info.types
  method get_shape id = ID.Map.find id info.shapes
  method get_storage id = ID.Map.find id info.storages
  method has_id (id:ID.t) = ID.Set.mem id info.ids

  method value_shape {value} = match value with
    | Var id -> self#get_shape id
    | FixDim(arr, dim, idx) ->
      let arrShape = self#value_shape arr in
      remove_pos_from_list (ImpHelpers.get_const_int dim) arrShape
    | VecConst ns -> [SymbolicShape.Const (List.length ns)]
    | VecSlice (_, _, nelts) -> [SymbolicShape.Const nelts]
    | _ -> SymbolicShape.scalar

  method value_storage {value} = match value with
    | Var id -> self#get_storage id
    | VecSlice _
    | FixDim _
    | Slice _ -> Imp.Alias
    | _ -> Imp.Local

  method mk_temp valNode =
    (*Printf.printf "[ImpBuilder.mk_temp] %s\n%!"
      (Imp.value_node_to_str valNode)
    ;
    *)
    let shape = self#value_shape valNode in
    let temp = self#fresh_local "temp" ~shape valNode.value_type in
    let rhs = self#flatten_rhs valNode in
    DynArray.add stmts $ Set(temp, rhs);
    temp

(* nested values on the RHS should be constants or variables *)
  method flatten_simple valNode =
    if is_simple valNode then valNode
    else self#mk_temp valNode

  (* LHS of assignment should be either variable, vecslice, or idx *)
  method flatten_lhs valNode =
    (*
    Printf.printf "[ImpBuilder.flatten_lhs] %s\n%!"
      (Imp.value_node_to_str valNode)
    ;
    *)
    match valNode.value with
    | CudaInfo _
    | Const _
    | VecConst _ -> failwith "Constants not allowed on LHS of assignment"
    | Var _ -> valNode
    | Idx (arr, indices) ->
      let arr' = self#flatten_simple arr in
      let indices' = List.map self#flatten_simple indices in
      {valNode with value = Idx(arr', indices')}
    | VecSlice (arr, idx, len) ->
      let arr' = self#flatten_simple arr in
      let idx' = self#flatten_simple idx in
      {valNode with value = VecSlice(arr', idx', len)}
    | _ -> self#mk_temp valNode

  (* RHS of an assignment *)
  method flatten_rhs valNode =
    (*Printf.printf "[ImpBuilder.flatten_rhs] %s\n%!"
      (Imp.value_node_to_str valNode)
    ;
    *)
    Imp.recursively_apply ~delay_level:1 self#flatten_simple valNode

  method flatten stmt : stmt =
    Imp.recursively_apply_to_stmt
      ~lhs:self#flatten_lhs
      ~rhs:self#flatten_rhs
      stmt

  method append stmt : unit =
    (*Printf.printf "[ImpBuilder.append] Adding %s\n%!"
      (Imp.stmt_to_str stmt)
    ;
    *)
    DynArray.add stmts (self#flatten stmt)

  method concat_list stmts = List.iter self#append stmts
  method concat (other:builder) = self#concat_list other#to_list

  method declare (id:ID.t) ?storage ?shape (ty:ImpType.t) =
    let storage = match storage with
      | Some storage -> storage
      | None ->
        if ImpType.is_scalar ty then Imp.Local
        else failwith $ Printf.sprintf
          "[ImpBuilder] Variable %s : %s cannot be declared without storage"
          (ID.to_str id)
          (ImpType.to_str ty)
    in
    let shape = match shape with
      | Some shape -> shape
      | None ->
        (match ty with
          | ImpType.ScalarT _ -> SymbolicShape.scalar
          | ImpType.VectorT(_, nelts) -> [SymbolicShape.Const nelts]
          | _ ->
            failwith $ Printf.sprintf
              "[ImpBuilder] Variable %s : %s cannot be declared without shape"
              (ID.to_str id)
              (ImpType.to_str ty)
        )
    in
    IFDEF DEBUG THEN
      if ImpType.rank ty <> SymbolicShape.rank shape then
        failwith $
          Printf.sprintf
            "[ImpBuilder] Mismatch for %s: type %s incompatible with shape %s"
              (ID.to_str id)
              (ImpType.to_str ty)
              (SymbolicShape.to_str shape)
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

  method fresh_local name ?storage ?shape (ty:ImpType.t) : value_node =
    let id = ID.gen_named name in
    let storage = match storage with Some s -> s | None -> Imp.Alias in
    self#declare  id ~storage ?shape ty;
    { value = Var id; Imp.value_type = ty }

  method cast name (v:value_node) (ty:ImpType.t) =
    if v.value_type <> ty then v
    else (
      let temp : Imp.value_node = self#fresh_local name ty in
      self#append $Set(temp, {Imp.value_type = ty; value = Imp.Cast(ty, v)});
      temp
    )

  method var (id:ID.t) : value_node =
    if not $ self#has_id id
    then failwith $ "[ImpBuilder] ID not found: " ^ ID.to_str id
    else
    let ty = self#get_type id in
    { value = Imp.Var id; value_type = ty }

  (* recursively build fixdim nodes for a list of indices *)
  method fixdims ~arr ~dims ~indices : value_node =
    let rec helper ?(counter=0) arr dims indices =
      match dims, indices with
      | [], [] -> arr
      | [_], [i] when ImpType.rank arr.value_type = 1 -> ImpHelpers.idx arr [i]
      | d::ds, i :: is ->
        let rhs = ImpHelpers.fixdim arr d i in
        let rhsShape = self#value_shape rhs in
        let rhsType = ImpType.peel arr.value_type in
        let lhs =
          self#fresh_local
            "fixed_slice" ~storage:Imp.Alias ~shape:rhsShape rhsType
        in
        self#append $ Imp.Set(lhs, rhs);
        (* adjust dims in case in order. *)
        (* For example, if we say fixdims x [0,1] [100,200] *)
        (* we want this to become fixdim(fixdim(x,0,100),0,200) *)
        let ds' =
          if d <= counter then List.map (fun d -> d - 1) ds
          else  ds
        in
        helper lhs ~counter:(counter+1) ds' is

        | _ -> failwith "Expected dims and indices to be of same length"
    in
    helper arr dims indices

  method idx_or_fixdims
      ~(arr:value_node)
      ~(dims:value_nodes)
      ~(indices:value_nodes) =
    let nIndices = List.length indices in
    let arrT = arr.value_type in
    let rank = ImpType.rank arrT in
    IFDEF DEBUG THEN
      if not (List.for_all ImpHelpers.is_const_int dims) then
        failwith $
          Printf.sprintf
            "Expected dims to be constant: %s"
            (Imp.value_nodes_to_str dims)
    ENDIF;
    let dims = List.map ImpHelpers.get_const_int dims in
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
    else if rank = nIndices then ImpHelpers.idx arr ~dims  indices
    else self#fixdims arr dims indices

  method build_add (name:string) (x:value_node) (y:value_node) : value_node =
    let tx = x.value_type in
    let ty = y.value_type in
    if tx <> ty then
      failwith $ Printf.sprintf
        "[ImpBuilder] Expected arguments to add to have same types (%s != %s)"
        (ImpType.to_str tx)
        (ImpType.to_str ty)
    else (
      let result = self#fresh_local name tx in
      self#append $ Set(result, ImpHelpers.add x y);
      result
    )

  method inline (impFn:Imp.fn) (inputs:value_nodes) (outputs:value_nodes) =
    let simpleInputs =  List.map self#flatten_simple inputs in
    let simpleOutputs = List.map self#flatten_simple outputs in
    let nonlocalEnv =
      ID.Map.of_lists
        (impFn.input_ids @ impFn.output_ids)
        (simpleInputs @ simpleOutputs)
    in
    let rec rewrite_dim = function
      | SymbolicShape.Dim(id, axis) ->
        (* assume shapes can only refer to input variables *)
        let newVal = ID.Map.find id nonlocalEnv in
        List.nth (self#value_shape newVal) axis
      | SymbolicShape.Op(op, x, y) ->
        SymbolicShape.Op(op, rewrite_dim x, rewrite_dim y)
      | SymbolicShape.Const n -> SymbolicShape.const n
    in
    let rewrite_shape shape = List.map rewrite_dim shape in
    let newShapeEnv = ID.Map.map rewrite_shape impFn.Imp.shapes in
    let rewrite_storage_helper id oldStorage =
      if ID.Map.mem id nonlocalEnv then
        self#value_storage (ID.Map.find id nonlocalEnv)
      else oldStorage
    in
    let newStorageEnv = ID.Map.mapi rewrite_storage_helper impFn.Imp.storage in
    let rename_local oldId =
      let name = ID.get_original_prefix oldId in
      let t = ID.Map.find oldId impFn.Imp.types in
      let shape = ID.Map.find oldId newShapeEnv in
      let storage = ID.Map.find oldId newStorageEnv in
      self#fresh_local name ~storage ~shape t
    in
    let newLocalVars = List.map rename_local impFn.local_ids in
    let replaceEnv =
      ID.Map.extend nonlocalEnv impFn.local_ids newLocalVars
    in
    let newBody = ImpReplace.replace_block replaceEnv impFn.body in
    self#concat_list newBody;
    List.iter2
      (fun originalOutput simpleOutputVar ->
         if not (is_simple originalOutput) then
          self#append $ Set(originalOutput, simpleOutputVar)
      )
      outputs
      simpleOutputs
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
