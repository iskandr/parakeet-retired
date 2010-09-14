

open Base
open DataflowGraph
open DynType 
open OptimizeDfg 


let buildup() : DataflowGraph.dfg * DataflowGraph.node_id = 
  let g = create_graph [IntT; IntT] [IntT] in
  let inSrc1 = input_source g 0  in   
  let inSrc2 = input_source g 1  in
  let plusNode =  add_binop g ~op:Prim.Add ~ty:IntT inSrc1 inSrc2 in  
  wire_to_graph_output g (mk_src plusNode.id 0) 0;
  g, plusNode.id
;;

let _ = Test.add_module "OptimizeDfg" [ ]
