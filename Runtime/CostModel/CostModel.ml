(* pp: -parser o pa_macro.cmo *)

open Base  
open SSA 


type shapes = Shape.t list
 
class type backend_costs = object
  method map_cost : FnTable.t -> SSA.fundef -> shapes -> shapes -> float
  method reduce_cost : FnTable.t -> SSA.fundef -> shapes -> shapes -> float 
  method scan_cost : FnTable.t -> SSA.fundef -> shapes -> shapes -> float
end 

(* problem: backends are parameterized by pointer/value type--- *)
(* how to return single backend type?*)
(* POSSIBLE SOLUTION: return the name? *) 
class type backend_runtime = object 
  method map : FnTable.t -> SSA.fundef ->  values -> values -> values 
  method reduce : FnTable.t -> SSA.fundef ->  values -> values -> values
  method scan : FnTable.t -> SSA.fundef ->  values -> values -> values
  method where : value -> value 
  method index : value -> value -> value
end 


type compute_location = backend_costs option  
type cost = float 

type description = Type.t * Shape.t  

let describe_value (shapeEnv : Shape.t ID.Map.t) (valNode : SSA.value_node) =
    let shape = match valNode.value with
      | Var id -> ID.Map.find id shapeEnv
      | _ -> Shape.scalar_shape
    in valNode.value_type, shape 
    
let rec describe_values shapeEnv = function  
    | [] -> []
    | v::vs -> 
        let rest = describe_values shapeEnv vs in 
        (describe_value shapeEnv varSets v) :: rest 

let get_shape (t, s, backendIds) = s 
let get_shapes argInfo = List.map get_shape argInfo 

(* NOTE: this assumes we're mapping over only the outermost axis! *)   
(* split args into max dim, and peeled inner args *) 
let split_args args =
    let maxShape = match Shape.max_shape_list (get_shapes args) with 
    | Some maxShape -> maxShape
    | None -> failwith "[CostModel] Argument shape error"
    in 
    assert (Shape.rank maxShape > 0);
    let peeler (ty,shape,gpuSet) =
      if Shape.eq shape maxShape then 
        Type.peel ty, Shape.peel shape, gpuSet  
      else  ty, shape, gpuSet 
    in  
    Shape.get maxShape 0, List.map peeler args 

let peel_arg ?(axes=[0]) (ty,shape) = Type.peel ~axes ty, Shape.peel ~axes shape
   
let peel_args ?(axes=[0]) args  = List.map peel_arg ~axes args 
   
(*
let val_node_on_gpu gpuSet valNode = match valNode.value with 
    | Var id -> ID.Set.mem id gpuSet 
    | _ -> true 
 

  (* since shape information has already been fully inferred in the *)
  (* ShapeInference module, phi nodes only have to merge information *)
  (* flow about whether a piece of data is on the GPU.  *)
  let phi_node varSets phiNode = 
    let leftOnGpu = val_node_on_gpu gpuSet phiNode.phi_left in 
    let rightOnGpu = val_node_on_gpu gpuSet phiNode.phi_right in 
    if leftOnGpu && rightOnGpu then ID.Set.add phiNode.phi_id gpuSet else gpuSet  
  
  
  let rec phi_nodes gpuSet = function 
    | [] -> gpuSet
    | node::nodes -> 
      let gpuSet' = phi_node gpuSet node in 
      phi_nodes gpuSet' nodes 
*)
let rec block_cost 
        (fnTable : FnTable.t) 
        (shapeEnv : Shape.t ID.Map.t) 
        (block : SSA.block) : cost  =
    (* start with zero cost...and add up all the stmtCosts *) 
    let totalCost = ref 0. in 
    for i = 0 to Block.length block - 1 do
        let stmtNode = Block.idx block i in 
        let currCost = stmt_cost fnTable shapeEnv stmtNode in
        totalCost := !totalCost + currCost;
    done;
    !totalCost
    
  and stmt_cost 
        (fnTable : FnTable.t) 
        (shapeEnv : Shape.t ID.Map.t)
        (gpuSet : ID.Set.t) 
        (stmtNode : SSA.stmt_node) : cost * ID.Set.t = match stmtNode.stmt with 
    | Set(_, rhs) -> 
        let _, cost  = exp_cost fnTable shapeEnv gpuSet rhs in cost, gpuSet
    | SetIdx(_, indices, _) -> float_of_int (List.length indices), gpuSet   
    | WhileLoop (condBlock, _, body, header) -> 
        let gpuSet' = phi_nodes gpuSet header in 
        let condCost, condGpuSet = 
          block_cost fnTable shapeEnv gpuSet' condBlock 
        in 
        let bodyCost, bodyGpuSet = 
          block_cost fnTable shapeEnv condGpuSet body 
        in
        let totalCost = condCost +. bodyCost in 
        totalCost, bodyGpuSet 
    | If(_, tBlock, fBlock, merge) -> 
        let tCost, tGpuSet = block_cost fnTable shapeEnv gpuSet tBlock in 
        let fCost, fGpuSet = block_cost fnTable shapeEnv tGpuSet fBlock in 
        let finalGpuSet = phi_nodes fGpuSet merge in 
        let totalCost = tCost +. fCost in 
        totalCost, finalGpuSet 
and exp_cost 
      (fnTable : FnTable.t) 
      (shapeEnv : Shape.t ID.Map.t) 
      (expNode : SSA.exp_node) : compute_location * cost = 
  match expNode.exp with 
    | Map(closure, axes, args) ->
      let fundef = FnTable.find closure.closure_fn fnTable in
      let closureArgs : description list = 
        describe_values shapeEnv closure.closure_args in
      let args : description list = describe_values shapeEnv args in     
      map_cost fnTable fundef axes closureArgs args 
    | _ -> None, 1.



(* TODO: loop over backends! *) 
and map_cost 
      (backends : backend list) 
      (fnTable : FnTable.t) 
      (fn : SSA.fundef)
      (axes : int list)
      (closureArgs : description list)
      (args : description list) : compute_location * cost =
    let first_shape = snd (List.hd args) in
    let axis_dims = List.map  (fun idx -> Shape.get first_shape idx) axes in
    let num_map_elts = List.prod axis_dims in 
    let nestedArgs = peel_args args in  
    let nestedCost = call_cost fnTable fn (closureArgs @ nestedArgs) in   
    let cpuCost = 1. +. (float_of_int num_map_elts) *. nestedCost in
    let bestCost = ref cpuCost in
    let bestBackend = ref None in
    let closureArgShapes = get_shapes closureArgs in  
    let argShapes = get_shapes args in  
    String.Map.iter
        (fun backend -> 
            let cost = backend#map_cost fnTable fn closureArgShapes argShapes in 
            if cost < !bestCost then begin 
                bestCost := cost; 
                bestBackend := Some backend
            end)
        backends 
    ; 
    !bestBackend, !bestCost
    
  and reduce_cost
      (backends : backend list) 
      (fnTable : FnTable.t) 
      (fn : SSA.fundef)
      (axes : int list)
      (closureArgs : description list)
      (args : description list) : compute_location * cost = 
        
        ???
        
        ~fnTable 
        ~init 
        ~initClosureArgs 
        ~fn 
        ~closureArgs 
        ~initArgs 
        ~args =   
    let gpuCost : cost = 
      GpuCost.reduce 
        ~fnTable 
        ~init
        ~initClosureArgs:(get_shapes initClosureArgs)
        ~fn 
        ~closureArgs:(get_shapes closureArgs) 
        ~initArgs:(get_shapes initArgs) 
        ~args:(get_shapes args) 
    in
    let maxDim, nestedArgs = split_args args in
    (* for now only compute call cost to initializer function and assume
       it has a comparable cost to the actual reducer 
     *)    
    let allNestedInputs = initClosureArgs @ initArgs @ nestedArgs in  
    let nestedCost = call_cost fnTable init allNestedInputs in
    
    let cpuCost = 1. +. (float_of_int maxDim) *. nestedCost in
    IFDEF DEBUG THEN 
      Printf.printf "Computed REDUCE cost: GPU - %f, HOST: %f\n" gpuCost cpuCost; 
    ENDIF; 
    if cpuCost < gpuCost then CPU, cpuCost 
    else GPU, gpuCost  
  
  
and call_cost fnTable fn argInfo  : cost =
  IFDEF DEBUG THEN
    let nArgs = List.length argInfo in 
    let nFormals = List.length fn.input_ids in    
    if nArgs  <> nFormals then
      let errStr = Printf.sprintf 
        "[CostModel] call_cost for %s got %d arguments but %d are required"
        (FnId.to_str fn.fn_id)
        nArgs
        nFormals
      in failwith errStr;
  ENDIF; 
  let shapes = get_shapes argInfo in 
  let shapeEnv = ShapeEval.eval_ssa_shape_env fnTable fn shapes in 
  block_cost fnTable shapeEnv gpuSet fn.body 
    
(* for now just run everything on the hosts *) 
let array_op op argVals = 
    match op, argVals with | _ -> None, 0.   