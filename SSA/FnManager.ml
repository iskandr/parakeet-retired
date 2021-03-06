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
  let key : UntypedSSA.value * Signature.t = 
    (untypedVal, signature) 
  in
  Hashtbl.add 
    state.specializations key typedFn.TypedSSA.fn_id

let maybe_get_specialization v signature =
  if Hashtbl.mem state.specializations (v, signature) then
    Some (Hashtbl.find state.specializations (v, signature))
  else None

let is_untyped_function untypedId =
  Hashtbl.mem state.untyped_functions untypedId

let get_untyped_function untypedId =
  Hashtbl.find state.untyped_functions untypedId
    
let get_untyped_args untypedId = 
  let fn = get_untyped_function untypedId in 
  fn.UntypedSSA.inputs 
    
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
  
let typed_input_names fnId = 
  let fn = get_typed_function fnId in 
  List.map ID.get_original_prefix fn.TypedSSA.input_ids 

