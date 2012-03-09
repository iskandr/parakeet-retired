(* pp: -parser o pa_macro.cmo *)

open Base
open Printf

(* A program is a mapping of function names to their functions. *)
(* These functions exist in both untyped and typed forms, as well as *)
(* a dataflow graph representations which should be eventually eliminated. *)
(* Functions are optimized before being inserted into the program. *)

type t = {
  untyped_functions : (FnId.t, UntypedSSA.fn) Hashtbl.t;

  typed_functions : FnTable.t;

  (* functions are either ID or Prim which gets specialized on either *)
  (* types or values (see Signature.ml)   *)
  specializations : (UntypedSSA.value * Signature.t, FnId.t) Hashtbl.t;

  name_to_untyped_id : (string, FnId.t) Hashtbl.t;

  untyped_id_to_name : (FnId.t, string) Hashtbl.t;
}

let create () =
  let n = 127 in
  {
    untyped_functions = Hashtbl.create n;
    typed_functions = FnTable.create n;
    specializations = Hashtbl.create n ;
    name_to_untyped_id = Hashtbl.create n;
    untyped_id_to_name = Hashtbl.create n;
  }

let state = create()

let add_untyped name fn =
  let id = UntypedSSA.fn_id fn in
  Hashtbl.add state.name_to_untyped_id name id;
  Hashtbl.add state.untyped_id_to_name id name;
  Hashtbl.add state.untyped_functions id fn

let rec add_untyped_list = function
  | [] -> ()
  | (name,fn)::rest ->
    add_untyped name fn;
    add_untyped_list rest
let add_untyped_map  fnMap =
  String.Map.iter add_untyped fnMap

let add_typed ?(optimize=true) fn =
  FnTable.add ~opt_queue:optimize fn state.typed_functions

let default_typed_optimizations =
  [
    (*"function cloning", TypedFunctionCloning.function_cloning;*)
    "simplify", Simplify.simplify_fn;
    "cse", CSE.cse;
    "inlining", Inline.run_fn_inliner;
  ]

let optimize_typed_functions () =
  (*Timing.start Timing.typedOpt;*)
  RunOptimizations.optimize_all_fns
    ~type_check:true
    ~maxiters:100
    state.typed_functions
    default_typed_optimizations
  ;
  IFDEF DEBUG THEN
    Printf.printf "*** Optimized all typed functions ***\n%!";
  ENDIF;
  ()
  (*;Timing.stop Timing.typedOpt*)

let get_untyped_name id = Hashtbl.find state.untyped_id_to_name id
let get_untyped_id name = Hashtbl.find state.name_to_untyped_id name

let get_typed_function_table () = state.typed_functions

let add_specialization
    ?(optimize=true)
    (untypedVal : UntypedSSA.value)
    (signature : Signature.t)
    (typedFn : TypedSSA.fn) =
  let fnId = typedFn.TypedSSA.fn_id in
  if not (FnTable.mem fnId state.typed_functions) then
   FnTable.add ~opt_queue:optimize typedFn state.typed_functions
  ;
  let key : UntypedSSA.value * Signature.t = (untypedVal, signature) in
  Hashtbl.add state.specializations key typedFn.TypedSSA.fn_id;
  IFDEF DEBUG THEN
    let untypedValStr =
      match untypedVal with
      | UntypedSSA.GlobalFn untypedId ->
        let fnName = Hashtbl.find state.untyped_id_to_name untypedId in
        Printf.sprintf
          "\"%s\" (untyped %s)" fnName (UntypedSSA.value_to_str untypedVal)
      | _ -> UntypedSSA.value_to_str untypedVal
    in
    let errorLog = TypeCheck.check_fn typedFn in
    if not $ Queue.is_empty errorLog then begin
      Printf.printf
        "\n --- Errors in specialization of %s for signature \"%s\"\n"
        untypedValStr
        (Signature.to_str signature)
      ;
      Printf.printf "%s\n" (TypedSSA.fn_to_str typedFn);
      TypeCheck.print_all_errors errorLog;
      failwith "Specialized function malformed"
    end
    else
      Printf.printf "\nSpecialized %s for signature \"%s\": \n %s \n"
        untypedValStr
        (Signature.to_str signature)
        (TypedSSA.fn_to_str typedFn)
  END

let maybe_get_specialization v signature =
  if Hashtbl.mem state.specializations (v, signature) then
    Some (Hashtbl.find state.specializations (v, signature))
  else None

let is_untyped_function untypedId =
  Hashtbl.mem state.untyped_functions untypedId

let get_untyped_function untypedId =
  Hashtbl.find state.untyped_functions untypedId

let get_typed_function typedId =
  FnTable.find typedId state.typed_functions

let get_typed_fn_from_value = function
  | UntypedSSA.GlobalFn fnId -> FnTable.find fnId state.typed_functions
  | _ -> failwith "expected a function"

let have_untyped_function name =
  Hashtbl.mem state.name_to_untyped_id name

let input_arity_of_untyped_fn fnId =
  UntypedSSA.FnHelpers.input_arity (get_untyped_function fnId)
let output_arity_of_untyped_fn fnId =
  UntypedSSA.FnHelpers.output_arity (get_untyped_function fnId)

let input_arity_of_typed_fn fnId =
  TypedSSA.FnHelpers.input_arity (get_typed_function fnId)
let output_arity_of_typed_fn fnId =
  TypedSSA.FnHelpers.output_arity (get_typed_function fnId)

let input_types_of_typed_fn fnId =
  TypedSSA.FnHelpers.input_types (get_typed_function fnId)
let output_types_of_typed_fn fnId =
  TypedSSA.FnHelpers.output_types (get_typed_function fnId)
