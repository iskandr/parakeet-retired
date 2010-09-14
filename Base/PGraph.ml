open Base

type graph = (int PSet.t) array
   
(* given a label->label set map, create a graph representation which is *)
(* an (int->int set) vector, along with a mapping back to label names *) 
  let create_from_labels (labelMap : ('a, 'a PSet.t) PMap.t) =
    let keys = Enum.map fst  (PMap.enum labelMap) in
    (* list of sets, to be merged with keys *) 
    let values = Enum.map snd (PMap.enum labelMap) in 
    let allNamesSet = Enum.fold PSet.union (PSet.of_enum keys) values in
    let allNamesList = PSet.to_list allNamesSet in 
    let idxToName = Array.of_list allNamesList in
    let n = Array.length idxToName in 
    let edges = Array.create n PSet.empty in 
    let nameToIdx =
       PMap.of_list (List.combine allNamesList $ List.til n) in
    let addEdge succName predName  =
      let predIdx = PMap.find predName nameToIdx in
      let succIdx = PMap.find succName nameToIdx in   
      edges.(predIdx) <- PSet.add succIdx edges.(predIdx);
    in
    for i = 0 to n-1 do  
      let name = idxToName.(i) in 
    let calls =
      if PMap.mem name labelMap 
      then PMap.find name labelMap 
      else PSet.empty 
    in  
      PSet.iter (addEdge name) calls       
    done;  
    idxToName, nameToIdx, edges 
    
  
  let topsort (edges: graph) = 
    let n = Array.length edges in
    let inDegrees = Array.create n 0 in   
    (* for every edge (x,y) add 1 to the in-degree of y *)
    let incrDeg succIdx = 
      inDegrees.(succIdx) <- inDegrees.(succIdx) + 1
    in 
    Array.iter (fun neighbors -> PSet.iter incrDeg neighbors) edges;
    let q = Queue.create () in 
    for i = 0 to n - 1 do 
      if inDegrees.(i) = 0 then Queue.add i q
    done;
    let decrDegree i = 
      inDegrees.(i) <- inDegrees.(i) - 1;
      if inDegrees.(i) = 0 then
        Queue.add i q
    in   
    let revOrder = ref [] in 
    while not $ Queue.is_empty q do 
      let curr = Queue.pop q in
      revOrder := curr :: !revOrder;  
      let succSet = edges.(curr) in
      PSet.iter decrDegree succSet 
    done;  
    List.rev !revOrder 
  
  let topsort_labels labelMap = 
    let idxToLabel, nameToLabel, edges = create_from_labels labelMap in 
    let order = topsort edges in 
    List.map (fun idx -> idxToLabel.(idx)) order  
       
