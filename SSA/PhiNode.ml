type 'a t = {
  phi_id : ID.t;
  phi_left:  'a;
  phi_right: 'a;
  phi_src : SrcInfo.t option;
}

let to_str value_node_to_str phiNode =
  sprintf "%s <- phi(%s, %s)"
    (ID.to_str phiNode.phi_id)
    (value_node_to_str phiNode.phi_left)
    (value_node_to_str phiNode.phi_right)


let mk ?src id left right =
  { phi_id = id; phi_left = left; phi_right = right; phi_src = src; }

(* make a block of phi nodes merging the three lists given *)
let rec mk_list ?src outIds leftVals rightVals =
  match outIds, leftVals, rightVals with
  | [], _, _ | _,[],_ | _,_,[] -> []
  | x::xs, y::ys, z::zs -> (phi ?src x y z) :: (phi_nodes ?src xs ys zs)

(* assume a block contains only phi, collect the IDs and
   either the left or right values
*)
let rec collect_phi_values chooseLeft = function
  | [] -> [], []
  | p::ps ->
    let ids, valNodes = collect_phi_values chooseLeft ps in
    let currVal = if chooseLeft then p.phi_left else p.phi_right in
    p.phi_id :: ids, currVal :: valNodes