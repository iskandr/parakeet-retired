open MapFusion 
open DynType 
open Printf 

module DFG = DataflowGraph

let buildup () = 
  let fn1Graph = DFG.create_graph [IntT; IntT] [IntT] in   
  let inSrc1 = DFG.input_source fn1Graph 0  in   
  let inSrc2 = DFG.input_source fn1Graph 1  in
  let plusNode = DFG.add_binop fn1Graph Prim.Add IntT inSrc1 inSrc2 in
  let plusId = plusNode.DFG.id in  
  DFG.wire_to_graph_output fn1Graph (DFG.mk_src plusId 0) 0;   
  let graph = 
    DFG.create_graph [VecT IntT; VecT IntT; VecT IntT] [VecT IntT] in
  let fnType = DynType.FnT ([IntT;IntT], [IntT]) in 
  let fn1 = DFG.add_exp graph (DFG.LocalFn fn1Graph)  in
  let fn1Src = DFG.mk_src fn1.DFG.id 0 in   
  let inVecSrc1 = DFG.input_source graph 0 in   
  let inVecSrc2 = DFG.input_source graph 1 in
  let mapDesc1 = { 
    DFG.fn_sources = [|fn1Src|]; 
    fn_types = [|fnType|]; 
    arg_sources = [|inVecSrc1; inVecSrc2|]; 
    arg_types = [|VecT IntT; VecT IntT|];
    output_types = [|VecT IntT|]
  } in   
  let map1 = DFG.add_exp graph (DFG.ArrayOp(Prim.Map, mapDesc1)) in 
  let map1Src = DFG.mk_src map1.DFG.id 0 in   
  let inVecSrc3 = DFG.input_source graph 2 in
  let mapDesc2 = {
    DFG.fn_sources = [|fn1Src|]; 
    fn_types = [|fnType|]; 
    arg_sources = [|map1Src; inVecSrc3|]; 
    arg_types = [|VecT IntT; VecT IntT|];
    output_types = [|VecT IntT|]
  } in   
  
  let map2 = DFG.add_exp graph (DFG.ArrayOp(Prim.Map, mapDesc2)) in 
  DFG.wire_to_graph_output graph (DFG.mk_src map2.DFG.id 0) 0;
  graph, map1, map2
;;

Test.add_module "Map Fusion" [ 
  Test.bracket "check valid map fusion"  ~buildup:buildup 
     (fun (g,map1,map2) -> MapFusion.map_fusion_valid g map1 map2
     );
    
  Test.diag_bracket "map fusion results" ~buildup:buildup    
     (fun (g,map1,map2) -> 
        let resultNode = MapFusion.map_fusion g map1 map2 in
        sprintf "Merging %s and  %s into \n %s"
          (DFG.node_to_str map1)
          (DFG.node_to_str map2)
          (DFG.node_to_str resultNode) 
     );
  (* fusion requires that the first map feed into the second or they share 
     common inputs. We can violate this constraint by reversing the order
     of the fused maps 
  *)
  Test.bracket "check invalid map fusion"  ~buildup:buildup 
     (fun (g,map1,map2) -> not (MapFusion.map_fusion_valid g map2 map1)
     );  
]