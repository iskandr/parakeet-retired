open Base
open Printf 


type space =
  | REG 
  | SREG 
  | CONST 
  | GLOBAL 
  | LOCAL 
  | PARAM 
  | SHARED 
  | SURF 
  | TEX
 
let gpu_space_to_str = function 
    | REG -> "reg"
    | SREG -> "sreg" 
    | CONST -> "const" 
    | GLOBAL -> "global"
    | LOCAL -> "local"
    | PARAM -> "param"
    | SHARED -> "shared"
    | SURF -> "surf"
    | TEX -> "tex"  

type symid = int
and typed_symbol = { id: symid; ptx_type: PtxType.ty ; space: space} 

type value =    
  | Sym of typed_symbol 
  | IntConst of Int64.t
  | FloatConst of float
  | Special of special_register
  (* careful not to nest these! *) 
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

let rec to_str symbols = function 
  | Sym {id=id; space=REG} ->  sprintf "%%%s" (Hashtbl.find symbols id) 
  | Sym {id=id} -> Hashtbl.find symbols id
  | IntConst i -> Int64.to_string i
  | FloatConst f -> Float.to_string f 
  | Special s -> special_register_to_str s
  | Vec2 (x,y) -> sprintf "{%s, %s}" (to_str symbols x) (to_str symbols y)
  | Vec4 (x,y,z,w) -> 
      sprintf "{%s, %s, %s, %s}"
        (to_str symbols x)
        (to_str symbols y)
        (to_str symbols z)
        (to_str symbols w)  
 
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


let get_id = function
  | Sym {id=id} -> id 
  | _ -> failwith "not a register"

let type_of_var = function 
  | Sym {ptx_type=t} -> t
  | _ -> failwith "[ptx_val->type_of_var] not a variable"
