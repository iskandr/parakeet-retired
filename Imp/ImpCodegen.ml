

class codegen = object (self)
    val mutable var_info : Imp.var_info ID.Map.t = ID.Map.empty 
    method fresh_input t : ID.t =
        let id = ID.gen() in 
        var_info <- ID.Map.add id t var_info; 
        id 
end
         