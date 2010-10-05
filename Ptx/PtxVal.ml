open Base
open Printf 

type symid = int 
type typed_symbol = { id: symid; ptx_type: PtxType.ty } 

type value =    
  | Reg of typed_symbol 
  | IntConst of Int64.t
  | FloatConst of float
  | Param of typed_symbol  
  | Special of special_register
   
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

let rec to_str symbols = function 
    | Reg {id=id} ->  sprintf "%%%s" (PMap.find id symbols) 
    | IntConst i -> Int64.to_string i
    | FloatConst f -> Float.to_string f 
    | Param {id=id} -> PMap.find id symbols
    | Special s -> special_register_to_str s  
 
and register_dim_to_str = function 
    | X -> "x"
    | Y -> "y"
    | Z -> "z"
and special_register_to_str = function
    | ThreadId d -> "%tid." ^ (register_dim_to_str d)
    | NumThreadId d -> "%ntid." ^ (register_dim_to_str d) 
    | ThreadLane -> "%laneid" 
    | WarpId -> "%warpid" 
    | CtaId d -> "%ctaid." ^ (register_dim_to_str d) 
    | NumCtaId d -> "%nctaid."  ^ (register_dim_to_str d)
    | ProcessorId -> "%smid" 
    | MaxProcessors -> "%nsmid"
    | GridId -> "%gridid" 
    | Clock -> "%clock"


let is_ptx_constant = function  
  | IntConst _ 
  | FloatConst _ -> true
  | _ -> false 

let type_of_special_register = function 
  | CtaId _
  | NumCtaId _
  | ThreadId _
  | NumThreadId _  
  | GridId
  | WarpId -> PtxType.U16
  | ThreadLane
  | ProcessorId
  | MaxProcessors
  | Clock  -> PtxType.U32

let type_of_var = function 
  | Param {ptx_type=ptx_type} 
  | Reg {ptx_type=ptx_type} -> ptx_type
  | _ -> failwith "[ptx_val->type_of_var] not a variable"
