(*
 * Module for debugging/testing the OCaml/C/Q interface.
 *
 * (c) Eric Hielscher, Alex Rubinsteyn 2010.
 *)
open CodeTemplate
open DynType
open HostVal
open Int64
open Printf
open QType
open Shape
open String

(* C function for inspecting OCaml blocks *)
external inspect : 'a -> unit = "inspect_block"

let debug_inspect_ocaml_blocks () =
  inspect (VecT(VecT(VecT(Int32T))))

let debug_gen_module_template entries =
  printf "Test: gen_module_template\n";
  printf "-------------------------\n";
  printf "Num funcs: %d\n%!" (List.length entries);
  let rec aux_entries = function
  | [] -> Int64.one
  | (name, locals, globals, body)::rest_entries ->
    printf "Function name: %s\n" name;
    let rec aux_vars = function
    | [] -> ()
    | var::rest_vars ->
        printf "Var: %s\n" var;
        aux_vars rest_vars
    in aux_vars locals;
    aux_vars globals;
    printf "Body: %s\n\n%!" body;
    aux_entries rest_entries
  in aux_entries entries

let debug_get_function_template template func_name =
  printf "Test: get_function_template\n";
  printf "---------------------------\n";
  printf "Template address: %ld\n" template;
  printf "Function name: %s\n\n%!" func_name;
  Int64.one

exception UnknownKTypeNum
let aux_ktypenum_to_ocaml_type ktypenum =
  let ocamltype = ktypenum_to_ocaml_type ktypenum in
  match ocamltype with
  | BoolT  -> printf "Got BoolT for %s\n%!"  (string_of_int ktypenum)
  | Int32T -> printf "Got Int32T for %s\n%!" (string_of_int ktypenum)
  | Int64T -> printf "Got Int64T for %s\n%!" (string_of_int ktypenum)
  | FloatT -> printf "Got FloatT for %s\n%!" (string_of_int ktypenum)
  | SymT   -> printf "Got SymT for %s\n%!"   (string_of_int ktypenum)
  | _ -> raise UnknownKTypeNum

let debug_ktypenum_to_ocaml_type () =
  printf "Test: ktypenum_to_ocaml_type\n";
  printf "----------------------------\n";
  aux_ktypenum_to_ocaml_type 1;
  aux_ktypenum_to_ocaml_type 6;
  aux_ktypenum_to_ocaml_type 7;
  aux_ktypenum_to_ocaml_type 9;
  aux_ktypenum_to_ocaml_type 11;
  printf "\n"

(** Includes a built-in convention of interpreting the args parameter for
testing each of the three possible return values (Success, Pass, and Error):
 - To get back a Success value, pass in a list of integers.
 - To get back a Pass value, pass in a simple integer.
 - To get back an Error value, pass in a simple float. *)
exception InvalidArgs
let debug_run_template template globals args =
  debug_ktypenum_to_ocaml_type ();
  printf "Test: run_module_template\n";
  printf "-------------------------\n\n%!";
  match args with
  | [head] -> begin match head.host_t with
    | VecT(Int32T) ->
      Success head
    | Int32T ->
      Pass
    | FloatT ->
      Error "Errored yo.\n"
    | _ -> raise InvalidArgs
    end
  | _ -> raise InvalidArgs

let _ = Callback.register "debug_gen_module_template" debug_gen_module_template
let _ = Callback.register "debug_get_function_template"
  debug_get_function_template
let _ = Callback.register "debug_run_template" debug_run_template

(*
  ocaml_dyn_type_to_ktypenum   = caml_named_value("debug_dyn_type_to_ktypenum");
*)
