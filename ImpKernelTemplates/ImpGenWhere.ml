open Base
open DynType
open Imp
open ImpCodegen

let gen_gen_index () =
  let codegen = new imp_codegen in
  let input = codegen#fresh_input (VecT Int32T) in
  let output = codegen#fresh_output (VecT Int32T) in
  let tid = codegen#fresh_var Int32T in
  let id = codegen#fresh_var Int32T in
  let prev = codegen#fresh_var Int32T in
  codegen#emit [
    set prev (int 0);
    ifTrue (lt id (len input)) [
      ifTrue (gt id (int 0)) (set prev (idx input (sub id (int 1))));
      ifTrue (ne prev (idx input id)) (setidx output prev id)
    ]
  ];
  codegen#finalize
