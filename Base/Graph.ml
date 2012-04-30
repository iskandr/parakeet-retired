open BaseCommon

module Make (Label : ORD) = struct
  module LabelMap = BaseMap.Make(Label)
  module LabelSet = BaseSet.Make(Label)

  (* adjacency expressed as vertex -> vertex set *)
  module VertexSet = BaseSet.Make(struct type t = int let compare = compare end)
  type graph = VertexSet.t array

  (* given a label->label set map, create a graph representation which is *)
  (* an (int->int set) vector, along with a mapping back to label names *)
  let create_from_labels labelMap =
    let keys = LabelMap.key_set labelMap in
    (* list of sets, to be merged with keys *)
    let values = LabelMap.values labelMap in
    let allNamesSet = BaseList.fold_left LabelSet.union keys values in
    let allNamesList = LabelSet.to_list allNamesSet in
    let idxToName = Array.of_list allNamesList in
    let n = Array.length idxToName in
    let edges = Array.create n VertexSet.empty in
    let nameToIdx =
      LabelMap.of_list (BaseList.combine allNamesList $ BaseList.til n) in
    let addEdge succName predName  =
      let predIdx = LabelMap.find predName nameToIdx in
      let succIdx = LabelMap.find succName nameToIdx in
      edges.(predIdx) <- VertexSet.add succIdx edges.(predIdx);
    in
    for i = 0 to n-1 do
      let name = idxToName.(i) in
      let calls =
        if LabelMap.mem name labelMap
        then LabelMap.find name labelMap
        else LabelSet.empty
      in
      LabelSet.iter (addEdge name) calls
    done;
    idxToName, nameToIdx, edges

  let topsort (edges:graph) =
    let n = Array.length edges in
    let inDegrees = Array.create n 0 in
    (* for every edge (x,y) add 1 to the in-degree of y *)
    let incrDeg succIdx =
      inDegrees.(succIdx) <- inDegrees.(succIdx) + 1
    in
    Array.iter (fun neighbors -> VertexSet.iter incrDeg neighbors) edges;
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
      VertexSet.iter decrDegree succSet
    done;
    BaseList.rev !revOrder

    let topsort_labels labelMap =
      let idxToLabel, nameToLabel, edges = create_from_labels labelMap in
      let order = topsort edges in
      BaseList.map (fun idx -> idxToLabel.(idx)) order
end
