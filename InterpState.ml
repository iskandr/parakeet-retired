open Base 
open Printf 
open SSA

(* include these for now to keep from having to 
    rewrite the CQInterface Makefile
*)


(* A program is a mapping of function names to their functions.*)
(* These functions exist in both untyped and typed forms, as well as *)
(* a dataflow graph representations which should be eventually eliminated. *)
(* Functions are optimized before being inserted into the program *)  

type t = {
  untyped_functions : FnTable.t;
  
  typed_functions : FnTable.t;
  
  (* functions are either ID or Prim which gets specialized on either *)
  (* types or values (see Signature.ml)   *)    
  specializations : (value * Signature.t, FnId.t) Hashtbl.t;
    
  name_to_untyped_id : (string, FnId.t) Hashtbl.t;
  
  untyped_id_to_name : (FnId.t, string) Hashtbl.t;   
} 

let add_specialization 
    program 
    (untypedVal : SSA.value) 
    (signature : Signature.t) 
    (typedFundef : SSA.fundef) =
  let fnId = typedFundef.SSA.fn_id in 
  if FnTable.mem fnId program.typed_functions then (
    (* if function is already in the fntable, don't add it again
       but make sure it really is the same function 
    *) 
    IFDEF DEBUG THEN 
      assert (FnTable.find fnId program.typed_functions = typedFundef) 
    ENDIF; 
    ()
  )
  else FnTable.add typedFundef program.typed_functions
  ; 
  Hashtbl.add program.specializations (untypedVal, signature) typedFundef.fn_id;
  IFDEF DEBUG THEN
    let untypedValStr = 
      match untypedVal with 
      | GlobalFn untypedId ->
        let fnName = Hashtbl.find program.untyped_id_to_name untypedId in  
        Printf.sprintf 
          "\"%s\" (untyped %s)" fnName (SSA.value_to_str untypedVal)
      | _ -> SSA.value_to_str untypedVal
    in  
    let errorLog = TypeCheck.check_fundef typedFundef in
    if not $ Queue.is_empty errorLog then (
      print_string "\n --- ";
      Printf.printf 
        "Errors in specialization of %s for signature \"%s\"\n"
        untypedValStr 
        (Signature.to_str signature)
      ; 
      Printf.printf "%s\n" (SSA.fundef_to_str typedFundef);
      TypeCheck.print_all_errors errorLog;
      exit 1
    )
    else 
      Printf.printf "\nSpecialized %s for signature \"%s\": \n %s \n"
      untypedValStr
      (Signature.to_str signature)
      (SSA.fundef_to_str typedFundef)
  END 

let maybe_get_specialization program v signature = 
  if Hashtbl.mem program.specializations (v, signature) then 
    Some (Hashtbl.find program.specializations (v, signature))
  else None   

let get_untyped_function program untypedId =
  FnTable.find untypedId program.untyped_functions  

let get_typed_function program typedId =
  FnTable.find typedId program.typed_functions 

let get_typed_fundef_from_value program = function 
  | GlobalFn fnId -> FnTable.find fnId program.typed_functions  
  | Lam fundef -> fundef  
  | _ -> failwith "expected a function" 


let default_untyped_optimizations = 
  [
    "simplify", Simplify.simplify_fundef;  
    "elim dead code", ElimDeadCode.elim_dead_code; 
    "elim partial applications", ElimPartialApps.elim_partial_apps;
    "elim common subexpression", CSE.cse;
    "inlining", Inline.run_fundef_inliner;  
  ] 

let optimize_untyped_functions program = 
  RunOptimizations.optimize_all_fundefs 
    ~maxiters:100
    program.untyped_functions
    default_untyped_optimizations

        
open AdverbFusion 
let default_typed_optimizations = 
  [
    (*"function cloning", TypedFunctionCloning.function_cloning;*)   
    "simplify", Simplify.simplify_fundef; 
    "dead code elim", ElimDeadCode.elim_dead_code; 
    "adverb fusion", AdverbFusion.optimize_fundef; 
    "inlining", Inline.run_fundef_inliner;  
  ]  
  
let optimize_typed_functions program = 
  RunOptimizations.optimize_all_fundefs 
    ~type_check:true
    ~maxiters:100
    program.typed_functions
    default_typed_optimizations
    
    
let create_untyped fnNameMap fundefMap =
  (* no idea how to properly set the hashtbl sizes *) 
  let n = 127 in 
  let untyped = FnTable.create n in 
  let nameToId = Hashtbl.create n in 
  let idToName = Hashtbl.create n in
  String.Map.iter 
    (fun name id -> 
        Hashtbl.add nameToId name id; Hashtbl.add idToName id name
    )
    fnNameMap; 
  FnId.Map.iter 
    (fun id fundef ->
        IFDEF DEBUG THEN assert (id = fundef.SSA.fn_id); ENDIF;
        FnTable.add fundef untyped
    )
    fundefMap;    
  let program = {
     untyped_functions = untyped; 
     typed_functions = FnTable.create n; 
     specializations = Hashtbl.create n ; 
     name_to_untyped_id = nameToId; 
     untyped_id_to_name = idToName; 
  }
  in optimize_untyped_functions program; 
  program  
  
  
let get_untyped_name program id = Hashtbl.find program.untyped_id_to_name id
let get_untyped_id program name = Hashtbl.find program.name_to_untyped_id name

let get_typed_function_table program = program.typed_functions
let get_untyped_function_table program = program.untyped_functions  
