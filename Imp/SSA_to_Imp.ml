




let translate (fn:SSA.fn) (inputTypes:ImpType.t list) : Imp.fn = 
    let codegen = new ImpCodegen.codegen in 
    let imp_tenv = InferImpTypes.infer fn inputTypes in
    let imp_output_types = List.map ID.Map.
    let input_ids = List.map codegen#fresh_input inputTypes in      
    
    
