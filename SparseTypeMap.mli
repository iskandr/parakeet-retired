(* sparse type envrionment which doesn't store designated value *) 
type 'a t 
val find : 'a -> 'a t -> DynType.t
val add : 'a -> DynType.t -> 'a t -> 'a t 
val empty : 'a t  
