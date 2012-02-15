open Base
open Printf 


type ptx_space =
  | REG 
  | SREG 
  | CONST 
  | GLOBAL 
  | LOCAL 
  | PARAM 
  | SHARED 
  | SURF
  | TEX
 
val ptx_space_to_str : ptx_space -> string 
type symid = int
and typed_symbol = {id: symid; ptx_type: PtxType.ty; space: ptx_space}

type value = 
  | Sym of typed_symbol
  | IntConst of Int64.t
  | FloatConst of float
  | Special of special_register
  | Vec2 of value * value
  | Vec4 of value * value * value * value  
   
and dim = X | Y | Z
and special_register =
  | ThreadId of dim
  | NumThreadId of dim
  | ThreadLane
  | WarpId
  | CtaId of dim
  | NumCtaId of dim
  | ProcessorId
  | MaxProcessors
  | GridId
  | Clock

val to_str : (symid, string) Hashtbl.t -> value -> string 
val register_dim_to_str : dim -> string    
val special_register_to_str : special_register -> string 

val is_ptx_num : value -> bool 
val is_ptx_constant : value -> bool 

val type_of_special_register : special_register -> PtxType.ty  
  
val get_id : value -> symid 

val type_of_var : value -> PtxType.ty 
val type_of_value : value -> PtxType.ty 