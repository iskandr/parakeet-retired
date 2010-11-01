type if_gate = { 
  (* this IF generates the following set of identifiers, 
     whose values might be taken from either branch 
  *)  
  if_output_ids : ID.t list;
  true_ids : ID.t list; 
  false_ids : ID.t list 
}

type loop_gate = { 
  (* what variables are assigned in the body of this loop? *) 
  loop_ids : ID.t list; 
  
  (* from which external identifiers do internal variables 
     get their initial values 
  *) 
  loop_inputs : ID.t ID.Map.t;
  
  (* what variables visible after this loop are generated, and
     from which internal var do they get their value?  
  *)
  loop_outputs : ID.t ID.Map.t;
  
  (* when repeating the loop body, which variables at the end of the
     body feed back into variables at the beginning? 
  *) 
  loop_header : ID.t ID.Map.t;  
}