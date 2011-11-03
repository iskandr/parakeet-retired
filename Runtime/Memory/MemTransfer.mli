


type copy_fn = src:Int64.t -> dest:Int64.t -> nbytes:int -> unit 

val register : src:MemId.t -> dest:MemId.t -> copy_fn -> unit   

val copy : Ptr.t -> Ptr.t -> int -> unit 