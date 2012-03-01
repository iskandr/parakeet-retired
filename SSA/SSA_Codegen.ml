open Base
open TypedSSA

class codegen =
  object (self : 'a)

    val types = (ref ID.Map.empty :  Type.t ID.Map.t ref)
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
        if Type.rank from_type > Type.rank to_type then
          failwith "Cannot convert from vector to scalar"
        else (
          let castNode =
          {
            exp = Cast(to_type, valNode);
            exp_src = None;
            exp_types = [to_type]
          }
          in
          let freshId = self#fresh_var to_type in
          let stmtNode = TypedSSA.set [freshId] castNode in
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

    method finalize = Block.of_array (DynArray.to_array code)
end

(* creates a codegen with identifiers initialized for input and output types,*)
(* passed the varnames and codegen to a user-provided function *)
(* which emits SSA into the codegen. *)
(* Once the body is finished, wrap up the code and type environment *)
(* as a fundef *)
let mk_codegen_fn
      ?name
      (inputTypes : Type.t list)
      (outputTypes  : Type.t list)
      (constr : codegen -> value_node list -> value_node list -> unit)  =
  let codegen = new codegen in
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
  let _ = constr codegen inputVars outputVars in
  TypedSSA.mk_fn
    ?name
    ~body:codegen#finalize
    ~tenv:codegen#get_type_env
    ~input_ids:inputIds
    ~output_ids:outputIds





