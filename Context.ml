(* one hole contexts are useful for accumulating tree structures *)
(* during transformation like Administrative/Monadic normal form, *)
(* or closure conversion *)

module type IR = sig
	type node
	type value 
	val mk_value_node : value -> node 
end

module MkContext = functor(I : IR) -> struct 
	type context = I.node->I.node 
	let hole = fun e -> e
	let plug (c1:context) (c2:context) = fun e -> c1 (c2 e)
	let plug_final (cxt:context) (node:I.node) = cxt node 
    let plug_final_value (cxt:context) (v:I.value) = cxt (I.mk_value_node v)
    let plug_list contexts = List.fold_left plug hole contexts
end

module ANF_Context = MkContext(ANF)
module Core_Context = MkContext(Core)