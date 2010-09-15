val fold_optimizations :
  'a -> bool -> (string * ('a -> 'a * bool)) list -> 'a * bool
val run_all :
  ?iter:int -> int -> 'a -> (string * ('a -> 'a * bool)) list -> 'a
val optimize_block :
  ?maxIters:int ->
  ?inliner:('a -> 'a * bool) -> 'a -> (string * ('a -> 'a * bool)) list -> 'a
