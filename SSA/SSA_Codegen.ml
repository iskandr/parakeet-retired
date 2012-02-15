open Base
open SSA
open SSA_Helpers

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

    method finalize = Block.of_array (DynArray.to_array code)
end

(* creates a codegen with identifiers initialized for input and output types,*)
(* passed the varnames and codegen to a user-provided function *)
(* which emits SSA into the codegen. *)
(* Once the body is finished, wrap up the code and type environment *)
(* as a fundef *)
let mk_codegen_fn
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
  mk_fn
    ?name:None
    ~body:codegen#finalize
    ~tenv:codegen#get_type_env
    ~input_ids:inputIds
    ~output_ids:outputIds



let (:=) xs y = mk_set (List.map get_id xs) y
let (@@) fn args = mk_app fn args
let scalar_op op = mk_op (Prim.ScalarOp op)
let array_op op = mk_op (Prim.ArrayOp op)
let impure_op op = mk_op (Prim.ImpureOp op)

let print = impure_op Prim.Print

let plus = scalar_op Prim.Add
let minus = scalar_op Prim.Sub
let mul = scalar_op Prim.Mult
let div = scalar_op Prim.Div

let lt = scalar_op Prim.Lt
let lte = scalar_op Prim.Lte
let eq = scalar_op Prim.Eq

let zero = mk_num (ParNum.Int32 0l)
let one = mk_num (ParNum.Int32 1l)
let neg_one = mk_num (ParNum.Int32 (-1l))
let trueVal = mk_num (ParNum.Bool true)
let falseVal = mk_num (ParNum.Bool false)

let inf = mk_num (ParNum.Inf Type.Float32T)
let neginf = mk_num (ParNum.NegInf Type.Float32T)

let select = mk_op (Prim.ScalarOp Prim.Select)

let reduce = mk_op  (Prim.Adverb Prim.Reduce)
let map = mk_op (Prim.Adverb Prim.Map)
let allPairs = mk_op (Prim.Adverb Prim.AllPairs)

(*let where = mk_op (Prim.ArrayOp Prim.Where)*)
let index = mk_op (Prim.ArrayOp Prim.Index)
(*let til = mk_op (Prim.ArrayOp Prim.Til)*)
let find = mk_op (Prim.ArrayOp Prim.Find)
let dimsize = mk_op (Prim.ArrayOp Prim.DimSize)


let value x = mk_exp $ Values [x]
let values xs = mk_exp $ Values xs

let len x = dimsize @@ [x; zero]
let incr (x:ID.t) (y:value_node) = mk_set [x] (plus @@ [y;one])
let set_int (x:ID.t) (y:Int32.t) =
  mk_set [x] (mk_vals_exp [Num (ParNum.Int32 y)])

type vars = value_node array
(* helper function for creating functions *)

let fn
      ?name
      ~(inputs_types : Type.t list)
      ~(output_types : Type.t list)
      ?(local_types = [])
      (bodyConstructor : vars -> vars -> vars -> stmt_node list) =



  (* inputs *)
  let nInputs = List.length input_types in
  let inputIds = ID.gen_named_list "input" nInputs in
  let inputs = List.map (fun t id -> mk_var ~ty:t id) input_types inputIds in
  (* outputs *)
  let nOutputs = List.length output_types in
  let outputIds = ID.gen_named_list "output" nOutputs in
  let outputs = List.map (fun t id -> mk_var ~ty:t id) output_types outputIds in
  (* locals *)
  let nLocals = List.length local_types in
  let localIds = ID.gen_named_list "temp" nLocals in
  let locals = List.map (fun t id -> mk_var ~ty:t id) local_types localIds in
  let body =
    Block.of_list $ bodyConstructor
      (Array.of_list inputs)
      (Array.of_list outputs)
      (Array.of_list locals)
  in
  let tenv =
    ID.Map.of_lists
      (inputIds @ outputIds @ localIds)
      (input_types @ output_types @ local_types)
  in
  mk_fn
    ?name
    ~tenv
    ~input_ids:inputIds
    ~output_ids:outputIds
    ~body

(* special case for creating function with 1 input, 1 output *)
let untyped_fn1 constructor =
  let wrapper inputs outputs _ = constructor inputs.(0) outputs.(0) in
  fn ~input_types[Type.BottomT] ~output_types:[Type.BottomT] wrapper

(* 2 inputs, 1 output, 0 locals *)
let fn2 constructor =
  let wrapper inputs outputs _ =
    constructor inputs.(0) inputs.(1) outputs.(0)
  in
  fn ~input_types:[Type.BottomT] ~output_types:[Type.BottomT] wrapper