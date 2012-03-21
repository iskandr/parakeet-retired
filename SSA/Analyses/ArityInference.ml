
type arity_elt = ArityVar of string | ArityFixed of int
type arity_fn = arity_elt * arity_elt
(* arity can be either tuples e.g. "1.3 :: 1", "(1.2, 3.4) :: 2",
   simple functions like "+ :: 2->1",
   or higher order functions like "map :: forall m,n. (m->n) => m->n"
*)

type arity = | Data of arity_elt
             | FirstOrderFn of int * int
             | HigherOrderFn of string list * arity_fn * arity_fn
