

type copy_fn = src:Int64.t -> dest:Int64.t -> nbytes:int -> unit 

let copy_functions : (MemId.t * MemId.t, copy_fn) Hashtbl.t = Hashtbl.create 127

let register ~src ~dest copy_fn = 
  let pair = src, dest in 
  if Hashtbl.mem copy_functions pair then 
    failwith "Copy function already registered"
  else
    Hashtbl.add copy_functions pair copy_fn    

let copy srcPtr destPtr nbytes = 
  let srcId = srcPtr#memspace_id in 
  let destId = destPtr#memspace_id in 
  let copy_fn = Hashtbl.find copy_functions (srcId, destId) in 
  copy_fn ~src:srcPtr#addr ~dest:destPtr#addr ~nbytes
