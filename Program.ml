open Base 
open Printf 
(* A program is a mapping of function names to their functions.*)
(* These functions exist in both untyped and typed forms, as well as *)
(* a dataflow graph representations which should be eventually eliminated. *)
(* Functions are optimized before being inserted into the program *)  

type program = {
  untyped_functions : (ID.t, SSA.fundef) Hashtbl.t; 

  (* functions are either ID or Prim which gets specialized on either *)
  (* types or values (see Signature.ml)   *)    
  specializations : 
    (SSA.value * Signature.t, ID.t) Hashtbl.t;
  
  typed_functions : (ID.t, SSA.fundef) Hashtbl.t;
  
  cuda_code_cache : 
    (SSA.value * Signature.t, Cuda.compiled_kernel) Hashtbl.t; 
} 

(* given an ID.t -> untyped function mapping, create a fresh program *) 
let create_from_map fnMap = 
  let fnList = PMap.to_list fnMap in
  let nFunctions = List.length fnList in {   
     untyped_functions = Hashtbl.from_list fnList;
     specializations = Hashtbl.create nFunctions;
     typed_functions = Hashtbl.create nFunctions;  
     cuda_code_cache = Hashtbl.create (20 * nFunctions);  
  }

let default_untyped_optimizations = 
  [
    "simplify", UntypedSimplify.simplify; 
    "elim partial applications", UntypedElimPartialApps.elim_partial_apps;
    "elim dead code", UntypedElimDeadCode.global_elim;
    "elim common subexpression", UntypedSimpleCSE.cse; 
  ] 
  
let create_from_untyped_block 
    ?(optimizations=default_untyped_optimizations) code = 
  let optimized = 
    RunOptimizations.optimize_block
      ~inliner:UntypedInline.run_inliner 
      code 
      optimizations 
  in
  printf "[create_program] finding constant values \n";
  let constEnv = UntypedFindConstants.find_constants optimized in
  printf "[create_program] creating map of global function values \n";
  let functionMap = UntypedFindConstants.build_function_map constEnv in
  create_from_map functionMap  

let default_typed_optimizations = 
  [
    (*"function cloning", TypedFunctionCloning.function_cloning;*)   
    "elim dead code", TypedElimDeadCode.elim_dead_code;  
  ]  
 
let add_specialization 
    program ?(optimizations = default_typed_optimizations) 
    untypedVal signature typedFundef =
     
  let typedId = ID.gen() in 
  Hashtbl.add program.specializations (untypedVal, signature) typedId;
  Hashtbl.add program.typed_functions typedId typedFundef;
  typedId 

let maybe_get_specialization program v signature = 
  if Hashtbl.mem program.specializations (v, signature) then 
    Some (Hashtbl.find program.specializations (v, signature))
  else None   

let get_untyped_function program untypedId = 
  Hashtbl.find program.untyped_functions untypedId 

let get_typed_function program typedId = 
  Hashtbl.find program.typed_functions typedId

let get_typed_fundef_from_value program = function 
  | SSA.Var id -> Hashtbl.find program.typed_functions id 
  | SSA.Lam fundef -> fundef  
  | _ -> failwith "expected a function" 
 (*
let default_dfg_rules = 
    [| 
       "cleanup map temps", OptimizeDfg.cleanup_map_temporaries  
    |] 
 *)  
        (*
let create_dfg program ?(dfg_rules=default_dfg_rules) typedId =
  let typedFundef = Hashtbl.find program.typed_functions typedId in 
  let dfg = TypedCoreToDfg.create_dfg typedFundef in
  let _ = OptimizeDfg.optimize dfg_rules dfg in
  Hashtbl.add program.typed_dataflow_graphs typedId dfg;
  dfg 
  
  
      *)