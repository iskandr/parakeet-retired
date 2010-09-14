
open Base
open DataflowGraph
open DynType 

module TC = TypedCore 

let buildup() : DataflowGraph.dfg * DataflowGraph.node_id = 
  let g = create_graph [IntT; IntT] [IntT] in
  let inSrc1 = input_source g 0  in   
  let inSrc2 = input_source g 1  in
  let plusNode =  add_binop g ~op:Prim.Add ~ty:IntT inSrc1 inSrc2 in  
  wire_to_graph_output g (mk_src plusNode.id 0) 0;
  g, plusNode.id
;;

Test.add_module "SequentializeDfg" [
   
  Test.bracket "gen_pred_map"  ~buildup:buildup  
  (fun (g,plusId) ->
    let expectedMap =  NodeId.Map.from_list [
       g.output_node_id, NodeId.Set.singleton plusId;
       plusId, NodeId.Set.singleton g.input_node_id
      ] in
    let generatedMap = 
      SequentializeDfg.gen_pred_map g g.output_node_id in 
    generatedMap = expectedMap 
  );
  
  Test.bracket "nodeid_order"  ~buildup:buildup  
  (fun (g,plusId) ->
    let expectedOrder = [g.input_node_id;plusId;g.output_node_id] in
    expectedOrder = SequentializeDfg.nodeid_order g
  );
  
  Test.bracket "sequentialize"  ~buildup:buildup  
  (fun (g,_) ->
     let typedFn = SequentializeDfg.sequentialize g in 
     match typedFn.TC.body with 
     | [
        TC.Set(id1, TC.ScalarOp(Prim.Add, IntT, [TC.Var id2; TC.Var id3])); 
       ] -> 
          let inputIdSet = PSet.of_list typedFn.TC.input_ids in  
          PSet.equal inputIdSet (PSet.of_list [id2; id3]) && 
          typedFn.TC.output_ids = [id1]
     | _ -> false
  ) 
   
]
