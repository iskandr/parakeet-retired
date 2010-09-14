
type result = private 
  | ResultOk  
  | ResultFail  
  | ResultDiag of string
  | ResultAssertFail of string * int * int 
  | ResultExn of string
  | ResultBreakdownExn of string 
  | ResultBuildupExn of string 

type test_type = private | OkTest | Diag
 
type test = private {
  test_type : test_type;
  test_name : string;
  test_wrapper : unit -> result;
}

val noop : unit -> unit
val ignore : 'a -> unit


val ok : string -> (unit -> bool) -> test
val bracket : string -> buildup:(unit -> 'a) -> ?breakdown:('a -> unit) ->
         ('a -> bool) -> test

val diag : string -> (unit -> string) -> test
val diag_bracket : string -> buildup:(unit -> 'a) -> ?breakdown:('a -> unit) ->
         ('a -> string) -> test

val add_module : string -> test list -> unit
val run_tests : unit -> unit
