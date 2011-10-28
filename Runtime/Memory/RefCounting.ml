
type ref_state = { 
    counts : (DataId.t, int) Hashtbl.t;
    dead_ids : DataId.t list ref; 
} 

let ref_state = { 
  counts = Hashtbl.create 1001;
  dead_ids = ref []
}

let getref dataId = Hashtbl.find ref_state.counts dataId

let setref dataId count = Hashtbl.replace ref_state.counts dataId count

let remove dataId = Hashtbl.remove ref_state.counts dataId

let get_dead_ids () = !ref_state.dead_ids
let iter_dead_ids f = List.iter f !ref_state.dead_ids 
   

let incref_data_id (dataId: DataId.t) = setref dataId (getref dataId +1)

let decref_data_id (dataId: DataId.t) =
    let nrefs = getref dataId in
    if nrefs <= 1 then (
        ref_state.dead_ids := dataId :: !ref_state.dead_ids; 
        clear dataId    
    )
    else setref dataId (nrefs - 1)

let rec incref = function
  | Value.Data id -> incref_data_id  id
  | Value.Scalar _ -> ()
  | Value.Array arr -> Array.iter incref arr

let rec decref  = function
  | Value.Data id -> decref_data_id id
  | Value.Scalar _ -> ()
  | Value.Array arr -> Array.iter decref arr
