open Base
open Printf 

type symid = int 

type value =    
  | Reg of symid
  | IntConst of Int64.t
  | FloatConst of float
  | Param of symid 
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
    | Reg id ->  sprintf "%%%s" (PMap.find id symbols) 
    | IntConst i -> Int64.to_string i
    | FloatConst f -> Float.to_string f 
    | Param id -> PMap.find id symbols
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
  | NumThreadId _  -> PtxType.U16
  | ThreadLane
  | WarpId
  | ProcessorId
  | MaxProcessors
  | GridId
  | Clock  -> PtxType.U32