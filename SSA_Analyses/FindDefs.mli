
module DefLattice : sig
  type t = 
    | Val of SSA.value  
    | Def of SSA.exp * int * int 
    | Combine of t list 
    | Top 
    | Bottom

  val bottom : t
  val combine : t -> t -> t 
  val eq : t -> t -> bool   
end

val find_defs : SSA.fundef -> (ID.t, DefLattice.t) Hashtbl.t 
 