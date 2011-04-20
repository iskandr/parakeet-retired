open Base

type ast_info = { 
	mutable defs_local : string PSet.t;
	mutable defs_global : string PSet.t; 

  mutable reads_local : string PSet.t;
  mutable reads_global : string PSet.t; 
    		
	mutable writes_local : string PSet.t;
	mutable writes_global : string PSet.t; 
		
	(* write to disk, print to screen, read from internet, etc... *)
	mutable io : bool;
    
	mutable nested_functions : bool;
  mutable is_function : bool;
}

let mk_ast_info () = {
	defs_local = PSet.empty; 
  defs_global = PSet.empty; 
		
  reads_local = PSet.empty; 
  reads_global = PSet.empty; 
        	 
	writes_local = PSet.empty; 
	writes_global = PSet.empty; 		 
		  
	io = false; 
	nested_functions = false;
  is_function = false;
}

let combine_ast_info info1 info2 = {	
    defs_local = PSet.union info1.defs_local info2.defs_local;
    defs_global = PSet.union info1.defs_global info2.defs_global;
  
	reads_local = PSet.union info1.reads_local info2.reads_local;
	reads_global = PSet.union info1.reads_global info2.reads_global; 
		
	writes_local = PSet.union info1.writes_local info2.writes_local; 
	writes_global = PSet.union info1.writes_global info2.writes_global; 
		
	io = info1.io || info2.io; 
	nested_functions = info1.nested_functions || info2.nested_functions;
    is_function = info1.is_function || info2.is_function
}

open Printf
let str_set_to_str set =
    "(" ^ (String.concat ", " (PSet.elements set)) ^ ")" 
    
let to_str info = 
    sprintf "{ reads_global: %s; writes_global: %s; io: %s; is_fn: %s; writes_local: %s }"
       (str_set_to_str info.reads_global)
       (str_set_to_str info.writes_global)
       (Bool.to_string info.io) 
       (Bool.to_string info.is_function)
       (str_set_to_str info.writes_local)
       
