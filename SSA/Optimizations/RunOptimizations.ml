(* pp: -parser o pa_macro.cmo *)

open Base
open TypedSSA

type optimization = TypedSSA.fn -> TypedSSA.fn * bool

let assert_valid fn rel optName  =
  let errorLog = TypeCheck.check_fn fn in
  if not $ Queue.is_empty errorLog then (
      let msg =
        Printf.sprintf "Errors found in %s %s %s"
          (FnId.to_str fn.TypedSSA.fn_id)
          rel
          optName
      in

      Printf.printf "--- %s ---\n%!" msg;
      Printf.printf "Source: %s\n%!" (TypedSSA.PrettyPrinters.fn_to_str fn);
      Printf.printf "\n\n";
      TypeCheck.print_all_errors errorLog;
      failwith msg
  )

let rec fold_optimizations ?(type_check=false) fn lastChanged =
  function
  | (name, opt)::rest ->
      IFDEF DEBUG THEN
        if type_check then assert_valid fn "before" name
      ENDIF;
      let optimized, changed = opt fn in
      IFDEF DEBUG THEN
        if type_check then assert_valid optimized "after" name
      ENDIF;
      fold_optimizations optimized (changed || lastChanged)  rest
  | [] -> fn, lastChanged

let rec optimize_fn
      ?(type_check=true)
      ?(iter=1)
      ?(maxiters=100)
      (fn : TypedSSA.fn)
      (optimizations : (string * optimization) list) =
  let fn', changed =
    fold_optimizations ~type_check fn false optimizations
  in
  if changed && iter < maxiters then
    optimize_fn
      ~type_check
      ~iter:(iter+1)
      ~maxiters
      fn'
      optimizations
  else fn', iter

let default_optimizations : 
  (string * (TypedSSA.fn -> TypedSSA.fn * bool)) list = 
  [
    "simplify", Simplify.simplify_fn; 
    "cse", CSE.cse;
    "fusion", AdverbFusion.fusion;
    "inlining", Inline.run_fn_inliner;
  ]

(* update each function in the unoptimized queue of the FnTable *)
let optimize_all_fns
      ?(type_check=true)
      ?(maxiters=100)
      ?(optimizations = default_optimizations) 
      () =
  let fnTable = FnManager.get_typed_function_table() in 
  while FnTable.have_unoptimized fnTable do
    let fn = FnTable.get_unoptimized fnTable in
    IFDEF DEBUG THEN
      (*
      Printf.printf "[RunOptimizations] Starting to optimize: %s\n"
        (TypedSSA.fn_to_str fn)
      ;
      *)
      if type_check then  (
        let errorLog = TypeCheck.check_fn fn in
        if not $ Queue.is_empty errorLog then (
          Printf.printf
            "--- found errors in %s before optimization ---\n%s\n"
            (FnId.to_str fn.TypedSSA.fn_id)
            (TypedSSA.fn_to_str fn)
          ;
          TypeCheck.print_all_errors errorLog;
          exit 1
        )
      );
    ENDIF;
    let optimized, iters = optimize_fn ~type_check ~maxiters fn optimizations in
    IFDEF DEBUG THEN
      assert (fn.fn_id = optimized.fn_id);
      if iters > 1 then
        Printf.printf "--- %s modified after %d optimization iters:\n%s\n\n%!"
            (FnId.to_str fn.fn_id)
            iters
            (TypedSSA.fn_to_str optimized)
      ;
    ENDIF;
    FnTable.update optimized fnTable
  done
