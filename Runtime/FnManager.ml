(* pp: -parser o pa_macro.cmo *)

open Base 
open Printf 
open SSA


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


let create () =
  let n = 127 in 
  {
    untyped_functions = FnTable.create n; 
    typed_functions = FnTable.create n; 
    specializations = Hashtbl.create n ; 
    name_to_untyped_id = Hashtbl.create n; 
    untyped_id_to_name = Hashtbl.create n; 
  }

let state = create() 

let add_untyped ?(optimize=true) name fn = 
  let id = fn.SSA.fn_id in 
  Hashtbl.add state.name_to_untyped_id name id; 
  Hashtbl.add state.untyped_id_to_name id name;
  FnTable.add ~opt_queue:optimize fn state.untyped_functions

let curry2 f (x,y) = f x y 

let add_untyped_list ?(optimize=true) (fnList: (string * SSA.fn) list) =
  List.iter  (curry2 (add_untyped ~optimize)) fnList 

let add_untyped_map  ?(optimize=true) fnMap = 
  String.Map.iter (add_untyped ~optimize) fnMap 

let default_untyped_optimizations = 
  [
    "simplify", Simplify.simplify_fn;  
    "elim common subexpression", CSE.cse;
    "inlining", Inline.run_fn_inliner;  
  ] 

let optimize_untyped_functions () = 
  Timing.start Timing.untypedOpt; 
  RunOptimizations.optimize_all_fns 
    ~maxiters:100 
    state.untyped_functions 
    default_untyped_optimizations
  ; 
  Timing.stop Timing.untypedOpt 

let default_typed_optimizations = 
  [
    (*"function cloning", TypedFunctionCloning.function_cloning;*)   
    "simplify", Simplify.simplify_fn;
    "cse", CSE.cse;  
    "adverb fusion", AdverbFusion.optimize_fn;
    "inlining", Inline.run_fn_inliner;  
  ]  
  
let optimize_typed_functions () = 
  Timing.start Timing.typedOpt; 
  RunOptimizations.optimize_all_fns 
    ~type_check:true
    ~maxiters:100
    state.typed_functions
    default_typed_optimizations
  ;
  Timing.stop Timing.typedOpt      


let get_untyped_name id = Hashtbl.find state.untyped_id_to_name id
let get_untyped_id name = Hashtbl.find state.name_to_untyped_id name

let get_typed_function_table () = state.typed_functions
let get_untyped_function_table () = state.untyped_functions     

let add_specialization 
    ?(optimize=true)
    (untypedVal : SSA.value) 
    (signature : Signature.t) 
    (typedFn : SSA.fn) =
  let fnId = typedFn.SSA.fn_id in 
  if FnTable.mem fnId state.typed_functions then (
    (* if function is already in the fntable, don't add it again
       but make sure it really is the same function 
    *) 
    IFDEF DEBUG THEN 
      assert (FnTable.find fnId state.typed_functions = typedFn) 
    ENDIF; 
    ()
  )
  else FnTable.add ~opt_queue:optimize typedFn state.typed_functions
  ; 
  let key = (untypedVal, signature) in 
  Hashtbl.add state.specializations key typedFn.fn_id; 
  IFDEF DEBUG THEN
    let untypedValStr = 
      match untypedVal with 
      | GlobalFn untypedId ->
        let fnName = Hashtbl.find state.untyped_id_to_name untypedId in  
        Printf.sprintf 
          "\"%s\" (untyped %s)" fnName (SSA.value_to_str untypedVal)
      | _ -> SSA.value_to_str untypedVal
    in  
    let errorLog = TypeCheck.check_fn typedFn in
    if not $ Queue.is_empty errorLog then (
      print_string "\n --- ";
      Printf.printf 
        "Errors in specialization of %s for signature \"%s\"\n"
        untypedValStr 
        (Signature.to_str signature)
      ; 
      Printf.printf "%s\n" (SSA.fn_to_str typedFn);
      TypeCheck.print_all_errors errorLog;
      exit 1
    )
    else 
      Printf.printf "\nSpecialized %s for signature \"%s\": \n %s \n"
      untypedValStr
      (Signature.to_str signature)
      (SSA.fn_to_str typedFn)
  END 

let maybe_get_specialization v signature = 
  if Hashtbl.mem state.specializations (v, signature) then 
    Some (Hashtbl.find state.specializations (v, signature))
  else None   

let is_untyped_function untypedId = 
  FnTable.mem untypedId state.untyped_functions 

let get_untyped_function untypedId =
  FnTable.find untypedId state.untyped_functions  

let get_typed_function typedId =
  FnTable.find typedId state.typed_functions 

let get_typed_fn_from_value = function 
  | GlobalFn fnId -> FnTable.find fnId state.typed_functions  
  | _ -> failwith "expected a function" 

let have_untyped_function name = 
  Hashtbl.mem state.name_to_untyped_id name     

let get_untyped_arity fnId = 
  List.length (get_untyped_function fnId).input_ids 