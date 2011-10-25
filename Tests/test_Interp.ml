

Test.add_module "Interp" [
    Test.ok "simple math" (fun () ->
      
        let x = SSA_Helpers.mk_int32 3 in 
        let y = SSA_Helpers.mk_int32 4 in 
        let exp = 
            SSA_Helpers.mk_primapp Prim.Add ~output_types:Type.Int32T [x;y]
        in
        let result = Interp.eval_exp exp in     
        result = Value.Scalar (ParNum.of_int 7)   
    ) 
] 