open Base 
open Printf 

(* A program is a mapping of function names to their functions.*)
(* These functions exist in both untyped and typed forms, as well as *)
(* a dataflow graph representations which should be eventually eliminated. *)
(* Functions are optimized before being inserted into the program *)  

type program = {
  untyped_functions : FnTable.t;

  (* functions are either ID or Prim which gets specialized on either *)
  (* types or values (see Signature.ml)   *)    
  specializations : (SSA.value * Signature.t, ID.t) Hashtbl.t;
  
  typed_functions : FnTable.t; 
  
  cuda_code_cache : 
    (SSA.value * Signature.t, Cuda.cuda_module) Hashtbl.t; 
} 

(* given an ID.t -> untyped function mapping, create a fresh program *) 
let create_from_map fnMap = 
  let fnList = PMap.to_list fnMap in
  let nFunctions = List.length fnList in {   
     untyped_functions = FnTable.from_list fnList;
     typed_functions = FnTable.create nFunctions; 
     specializations = Hashtbl.create nFunctions;
     cuda_code_cache = Hashtbl.create (20 * nFunctions);  
  }

let default_untyped_optimizations = 
  [
    "simplify", UntypedSimplify.simplify_block; 
    "elim partial applications", UntypedElimPartialApps.elim_partial_apps;
    "elim dead code", UntypedElimDeadCode.global_elim;
    "elim common subexpression", UntypedSimpleCSE.cse; 
  ] 
  
let create_from_untyped_block 
    ?(optimizations=default_untyped_optimizations) code = 
  let optimized = 
    RunOptimizations.optimize_block
      ~inline:true
      (FnTable.create 1) (* we don't yet know about any global functions *) 
      code 
      optimizations 
  in
  debug (Printf.sprintf "Program body: %s \n" (SSA.block_to_str optimized)); 
  debug "[create_program] finding constant values \n";
  let constEnv = UntypedFindConstants.find_constants optimized in
  debug "[create_program] creating map of global function values \n";
  let functionMap = UntypedFindConstants.build_function_map constEnv in
  create_from_map functionMap  

let default_typed_optimizations = 
  [
    (*"function cloning", TypedFunctionCloning.function_cloning;*)   
    "elim dead code", TypedElimDeadCode.elim_dead_code;  
    "adverb fusion", AdverbFusion.optimize_fundef; 
  ]  
 
let add_specialization 
    program ?(optimizations = default_typed_optimizations) 
    untypedVal signature typedFundef =
  let typedId = FnTable.add  typedFundef program.typed_functions in  
  Hashtbl.add program.specializations (untypedVal, signature) typedId;
  typedId 

let maybe_get_specialization program v signature = 
  if Hashtbl.mem program.specializations (v, signature) then 
    Some (Hashtbl.find program.specializations (v, signature))
  else None   

let get_untyped_function program untypedId = 
  FnTable.find untypedId program.untyped_functions  

let get_typed_function program typedId = 
  FnTable.find typedId program.typed_functions 

let get_typed_fundef_from_value program = function 
  | SSA.Var id -> FnTable.find id program.typed_functions  
  | SSA.Lam fundef -> fundef  
  | _ -> failwith "expected a function" 
