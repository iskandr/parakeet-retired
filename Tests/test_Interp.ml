
(*
Test.add_module "Interp" [
    Test.ok "simple math" (fun () ->
      
        let x = SSA_Helpers.mk_int32 3 in 
        let y = SSA_Helpers.mk_int32 4 in
        let add = (Prim.ScalarOp Prim.Add) in  
        let exp = SSA_Helpers.mk_primapp add ~output_types:[Type.int32] [x;y] in
        let result = Interp.eval_exp exp in    
        let expected : Interp.value list =  [Value.Scalar (ParNum.of_int 7)]  in 
        result = expected 
    ) 
] 
*)