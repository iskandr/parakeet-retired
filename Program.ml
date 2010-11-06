open Base 
open Printf 
open SSA

(* include these for now to keep from having to 
    rewrite the CQInterface Makefile
*)
open AdverbFusion
open ElimDeadCode 
open UntypedElimPartialApps
open UntypedElimDeadCode
open UntypedSimpleCSE

(* A program is a mapping of function names to their functions.*)
(* These functions exist in both untyped and typed forms, as well as *)
(* a dataflow graph representations which should be eventually eliminated. *)
(* Functions are optimized before being inserted into the program *)  

type program = {
  untyped_functions : FnTable.t;

  typed_functions : FnTable.t;
  
  (* functions are either ID or Prim which gets specialized on either *)
  (* types or values (see Signature.ml)   *)    
  specializations : (value * Signature.t, FnId.t) Hashtbl.t;
  
  
  name_to_untyped_id : (string, FnId.t) Hashtbl.t;
  
  untyped_id_to_name : (FnId.t, string) Hashtbl.t;   
} 

let default_untyped_optimizations = 
  [
    "simplify", Simplify.simplify_fundef;  
    "elim dead code", ElimDeadCode.elim_dead_code; 
    (*"elim partial applications", UntypedElimPartialApps.elim_partial_apps;*)
    (*"elim common subexpression", UntypedSimpleCSE.cse;*) 
  ] 
  
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
        assert (id = fundef.SSA.fn_id);  
        (* fntable adds the identifier the fundef has been tagged with, 
           which hopefully agrees with the one we're ignoring here. 
           *** Important *** 
           FUNDEF GETS OPTIMIZED BEFORE BEING ADDED!
        *)
        let fundef' = 
          RunOptimizations.optimize_fundef
            ~maxiters:100
            ~inline:true  (* functions have been topologically sorted *)
            untyped
            fundef
            default_untyped_optimizations
        in     
        ignore (FnTable.add fundef' untyped)
    )
    fundefMap; 
  {
     untyped_functions = untyped; 
     typed_functions = FnTable.create n; 
     specializations = Hashtbl.create n ; 
     name_to_untyped_id = nameToId; 
     untyped_id_to_name = idToName; 
  }
 
let default_typed_optimizations = 
  [
    (*"function cloning", TypedFunctionCloning.function_cloning;*)   
    (*"elim dead code", TypedElimDeadCode.elim_dead_code;*)  
    "simplify", Simplify.simplify_fundef; 
    "dead code elim", ElimDeadCode.elim_dead_code; 
   (* "adverb fusion", AdverbFusion.optimize_fundef;*) 

  ]  
 
let add_specialization 
    program 
    ?(optimizations = default_typed_optimizations) 
    (untypedVal : SSA.value) 
    (signature : Signature.t) 
    (typedFundef : SSA.fundef) =
  let optimized = 
     RunOptimizations.optimize_fundef
      ~maxiters:10
      ~inline:true
      program.typed_functions 
      typedFundef
      optimizations
  in  
  let typedId = FnTable.add optimized program.typed_functions in  
  Hashtbl.add program.specializations (untypedVal, signature) typedId;
  IFDEF DEBUG THEN
    Printf.printf "Specialized %s for signature %s: \n %s\n%!"
      (SSA.value_to_str untypedVal)
      (Signature.to_str signature)
      (SSA.fundef_to_str optimized)
  END; 
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
  | GlobalFn fnId -> FnTable.find fnId program.typed_functions  
  | Lam fundef -> fundef  
  | _ -> failwith "expected a function" 
