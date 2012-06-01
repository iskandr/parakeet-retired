open Base 

type 'a formal_args = {
  names : string list; 
  defaults : (string * 'a) list; 
}

type 'a actual_args = {
  values : 'a list;
  keywords : (string * 'a) list;
}
val formal_args_from_lists : 
  string list -> string list -> 'a list -> 'a formal_args
val actual_args_from_lists : 
  'a list -> string list -> 'a list -> 'a actual_args

val of_names : string list -> 'a formal_args 
val of_values : 'a list -> 'a actual_args

val apply_to_formal_values : 
  ('a -> 'b) ->  'a formal_args -> 'b formal_args 

val apply_to_actual_values : 
  ('a -> 'b) ->  'a actual_args -> 'b actual_args

val all_formal_names : 'a formal_args -> string list 
val all_actual_values : 'a actual_args -> 'a list

val prepend_formal_names : string list ->  'a formal_args -> 'a formal_args
val prepend_actual_values : 'a list ->  'a actual_args -> 'a actual_args

val combine_actual_args : 'a actual_args -> 'a actual_args -> 'a actual_args 

val formal_args_to_str : 
    value_to_str:('a -> string) -> 
      'a formal_args -> 
        string

val actual_args_to_str : 
    value_to_str:('a -> string) -> 
      'a actual_args -> 
        string

val bind : 
  ?env:(string * 'a) list -> 
    'a formal_args -> 
      'a actual_args -> 
        (string * 'a) list 
