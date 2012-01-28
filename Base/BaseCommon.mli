include module type of Std

val ($) : ('a -> 'b) -> 'a -> 'b
val (>>=) : 'a -> ('a -> 'b) -> 'b


val ignore : 'a -> unit

val compose : ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)

val debug : string -> unit


exception SourcedError of string * SrcInfo.t
exception StaticError of string

val mk_gen : unit -> (unit -> int)
val all_pairs : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

module type ORD = sig
  type t
  val compare : t -> t -> int
end

val safe_div : int -> int -> int
