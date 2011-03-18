open Base
open SSA

class ssa_codegen =
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
      (inputTypes : DynType.t list) 
      (outputTypes  : DynType.t list) 
      (constr : ssa_codegen -> value_node list -> value_node list -> unit)  = 
  let codegen = new ssa_codegen in 
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
  SSA.mk_fundef 
    ~body:codegen#finalize 
    ~tenv:codegen#get_type_env
    ~input_ids:inputIds
    ~output_ids:outputIds 
    
    

let (:=) xs y = mk_set (List.map SSA.get_id xs) y 
let (@@) fn args = mk_app fn args  
let scalar_op op = mk_op (Prim.ScalarOp op)
let array_op op = mk_op (Prim.ArrayOp op)
let impure_op op = mk_op (Prim.ImpureOp op)

let print = impure_op Prim.Print 

let plus = scalar_op Prim.Add 
let minus = scalar_op Prim.Sub 
let mul = scalar_op Prim.Mult  

let lt = scalar_op Prim.Lt
let lte = scalar_op Prim.Lte
let eq = scalar_op Prim.Eq 

let zero = mk_num (PQNum.Int32 0l) 
let one = mk_num (PQNum.Int32 1l)
let trueVal = mk_num (PQNum.Bool true)
let falseVal = mk_num (PQNum.Bool false) 

let inf = mk_num (PQNum.Inf DynType.Float32T)
let neginf = mk_num (PQNum.NegInf DynType.Float32T)

let reduce = mk_op  (Prim.Adverb Prim.Reduce) 
let map = mk_op (Prim.Adverb Prim.Map)
let allPairs = mk_op (Prim.Adverb Prim.AllPairs) 

let where = mk_op (Prim.ArrayOp Prim.Where) 
let index = mk_op (Prim.ArrayOp Prim.Index) 
let til = mk_op (Prim.ArrayOp Prim.Til) 
let find = mk_op (Prim.ArrayOp Prim.Find)
let dimsize = mk_op (Prim.ArrayOp Prim.DimSize) 

let value x = SSA.mk_exp $ SSA.Values [x]
let values xs = SSA.mk_exp $ SSA.Values xs

let len x = dimsize @@ [x; zero]
let incr (x:ID.t) (y:value_node) = SSA.mk_set [x] (plus @@ [y;one])    
let set_int (x:ID.t) (y:Int32.t) = 
  SSA.mk_set [x] (SSA.mk_vals_exp [SSA.Num (PQNum.Int32 y)])
  
type vars = value_node array 
(* helper function for creating functions *) 
let mk_fn 
      (nInputs : int) 
      (nOutputs : int) 
      (nLocals : int) 
      (bodyConstructor : vars -> vars -> vars -> stmt_node list) =  
  let inputs = ID.gen_fresh_array nInputs in
  let inputVars = Array.map SSA.mk_var inputs in 
  let outputs = ID.gen_fresh_array nOutputs in
  let outputVars = Array.map SSA.mk_var outputs in
  let locals = ID.gen_fresh_array nLocals in 
  let localVars = Array.map SSA.mk_var locals in   
  let body = Block.of_list $ bodyConstructor inputVars outputVars localVars in 
  mk_fundef 
    ~input_ids:(Array.to_list inputs)
    ~output_ids:(Array.to_list outputs)
    ~tenv:ID.Map.empty 
    ~body  

(* special case for creating function with 1 input, 1 output *) 
let fn1 constructor =
  let constructorWrapper = 
    fun inputs outputs _ -> constructor inputs.(0) outputs.(0)
  in 
  mk_fn 1 1 0 constructorWrapper   