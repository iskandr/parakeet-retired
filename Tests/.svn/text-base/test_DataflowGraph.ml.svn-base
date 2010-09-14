open Base
open DataflowGraph
open DynType 

let buildup() : DataflowGraph.dfg * DataflowGraph.node_id = 
  let g = create_graph [IntT; IntT] [IntT] in
  let inSrc1 = input_source g 0  in   
  let inSrc2 = input_source g 1  in
  let plusNode =  add_binop g ~op:Prim.Add ~ty:IntT inSrc1 inSrc2 in  
  wire_to_graph_output g (mk_src plusNode.id 0) 0;
  g, plusNode.id
;;

Test.add_module "DataflowGraph" [ 
  Test.bracket "predecessor of (+) node should be Input node"  ~buildup:buildup  
  (fun (g,plusId) -> 
    PSet.equal (nodeid_pred_ids g plusId) (PSet.singleton g.input_node_id)); 
    
  Test.bracket "successor of (+) node should be Output node"  ~buildup:buildup  
  (fun (g,plusId) -> 
    PSet.equal  (nodeid_succ_ids g plusId) (PSet.singleton g.output_node_id))
]
