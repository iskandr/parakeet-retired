type label = int 
and ptx_comp =
    EQ | NE | LT | LE | GT | GE | LO | LS | HI | HS
  | EQU | NEU | LTU | LEU | GTU | GEU | NUM | NAN 
and ptx_bits = Wide | Low | High 
and ptx_rounding_mode = 
  | RoundNearest_Int
  | RoundZero_Int
  | RoundNegInf_Int
  | RoundPosInf_Int
  | RoundNearest
  | RoundZero
  | RoundNegInf
  | RoundPosInf
and ptx_float_mode =
  | Approx
  | Full
  | RN (* same as RoundNearest, but used for DIV operator *)
and ptx_unop =
  | Abs 
  | Cnot 
  | Neg 
  | Not
  | Rsqrt of ptx_float_mode
  | Cos of ptx_float_mode
  | Ex2 of ptx_float_mode
  | Lg2 of ptx_float_mode
  | Sin of ptx_float_mode
  | Sqrt of ptx_float_mode
  | Rcp of ptx_float_mode
and ptx_binop =
  | Add 
  | And 
  | Max 
  | Min 
  | Or
  | Rem 
  | Ret 
  | Sad 
  | Set 
  | Shl 
  | Shr 
  | Sub 
  | Xor
  | Setp of ptx_comp
  | FloatDiv of ptx_float_mode
  | FloatMul
  | IntDiv
  | IntMul of ptx_bits
  | Mul24 of ptx_bits
and geom = Tex1D | Tex2D | Tex3D
and ptx_op =
  | Unop of ptx_unop * PtxType.ty
  | Binop of ptx_binop * PtxType.ty
  | Mad of ptx_bits * PtxType.ty  
  | Mad24 of ptx_bits * PtxType.ty
  | FloatMad of PtxType.ty   
  | Cvt of PtxType.ty * PtxType.ty
  | Ld of PtxVal.ptx_space * PtxType.ty * int 
  | Selp of PtxType.ty 
  | Slct of PtxType.ty * PtxType.ty  
  | St of PtxVal.ptx_space * PtxType.ty * int 
  | Mov of PtxType.ty 
  | Bra of label
  | Comment of string 
  | Exit 
  | Bar of int 
  | Tex of geom * PtxType.ty
 
and var_decl = {
  t: PtxType.ty; 
  decl_space: PtxVal.ptx_space;
  array_size: int option;
  init_val: int option;
}

(* instructions can be conditional on a predicate, 
   its negation, or not at all
*) 
and guard = IfTrue of PtxVal.value | IfFalse of PtxVal.value | NoGuard  
and instruction = { 
  op: ptx_op; 
  sat : bool;
  ftz : bool;
  rounding : ptx_rounding_mode option; 
  args : PtxVal.value array; 
  label : label option; 
  pred : guard
}
  
and kernel = {
  params: (PtxVal.symid * PtxType.ty) array;
  decls: (PtxVal.symid, var_decl) Hashtbl.t; 
  symbols : (PtxVal.symid, string) Hashtbl.t; 
  code: instruction array; 
  textures: (PtxVal.symid * PtxType.ty) array;  
}

and ptx_compute_capability = SM_10 | SM_11 | SM_13 | SM_20 | SM_21
and ptx_module = {
  kernels : (string, kernel) PMap.t;
  compute_capability: ptx_compute_capability;
}

val module_from_named_kernels : (string * kernel) list -> ptx_module 

val is_float_rounding_mode : ptx_rounding_mode -> bool 

val is_int_rounding_mode : ptx_rounding_mode -> bool 

val num_to_geom : int -> geom 

(*********************************************************
                TO_STRING FUNCTIONS 
 *********************************************************)

val ptx_module_to_str : ptx_module -> string 

val add_kernel_to_buffer : Buffer.t -> string -> kernel -> unit
 
val kernel_to_str : kernel -> string -> string
  
val add_kernel_textures_to_buffer : 
      Buffer.t -> (PtxVal.symid, string) Hashtbl.t -> 
        (PtxVal.symid * PtxType.ty) array -> unit
        
val tex_to_str : (PtxVal.symid, string) Hashtbl.t -> PtxVal.symid  -> PtxType.ty -> string 

val add_kernel_params_to_buffer 
    : Buffer.t -> (PtxVal.symid, string) Hashtbl.t -> 
        (PtxVal.symid * PtxType.ty) array -> unit 
   
val param_to_str 
      : (PtxVal.symid, string) Hashtbl.t -> PtxVal.symid -> PtxType.ty -> string 

val add_kernel_decls_to_buffer 
    : Buffer.t -> (PtxVal.symid, string) Hashtbl.t -> 
        (PtxVal.symid, var_decl) Hashtbl.t -> unit 

val add_kernel_body_to_buffer 
    : Buffer.t -> (PtxVal.symid, string) Hashtbl.t -> instruction array -> unit 
    
val add_instruction_to_buffer 
    : Buffer.t -> (PtxVal.symid, string) Hashtbl.t -> instruction -> unit 

val ptx_op_name : ptx_op -> string
  
val ptx_op_args_to_buffer : 
      Buffer.t -> (PtxVal.symid, string) Hashtbl.t -> ptx_op -> 
        PtxVal.value array -> unit 

 
val ptx_bits_to_str : ptx_bits -> string  
    
val ptx_comp_to_str : ptx_comp -> string 
val ptx_unop_to_str : ptx_unop -> string 
val ptx_binop_to_str : ptx_binop -> string 

val ptx_label_to_str : string -> string 
val ptx_rounding_mode_to_str : ptx_rounding_mode -> string

val ptx_float_mode_to_str : ptx_float_mode -> string 
val compute_capability_to_str : ptx_compute_capability -> string 
val ptx_geom_to_str : geom -> string 


(****************************************************************
                    HELPER/SHORTCUT FUNCTIONS 
 ****************************************************************)
val int64 : Int64.t -> PtxVal.value 
val int : int -> PtxVal.value 
val float : float -> PtxVal.value 

val mkop : ptx_op -> PtxVal.value list -> instruction 

(* add label or predicate to instruction *)
val pred : PtxVal.value -> instruction -> instruction 
val pred_not : PtxVal.value -> instruction -> instruction 
val label : label -> instruction -> instruction 
val round : ptx_rounding_mode -> instruction -> instruction

val op0 : ptx_op -> instruction 
val op1 : ptx_op -> PtxVal.value -> instruction  
val op2 : ptx_op -> PtxVal.value -> PtxVal.value -> instruction 
val op3 : ptx_op -> PtxVal.value -> PtxVal.value -> PtxVal.value -> instruction 
val op4 : ptx_op -> PtxVal.value -> PtxVal.value -> 
            PtxVal.value -> PtxVal.value -> instruction 

val unop : ptx_unop -> PtxType.ty -> 
             dest:PtxVal.value -> src:PtxVal.value -> instruction
              
val binop : ptx_binop -> PtxType.ty -> dest:PtxVal.value -> 
              src1:PtxVal.value -> src2:PtxVal.value -> instruction 

val comment : txt:string -> instruction 

val mul : PtxType.ty -> dest:PtxVal.value -> 
            src1:PtxVal.value -> src2:PtxVal.value -> instruction
            
val div : PtxType.ty -> dest:PtxVal.value -> 
            src1:PtxVal.value -> src2:PtxVal.value -> instruction 
            
val mul_wide : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction

val mul_lo : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction
                
val mul_hi : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction
val add : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction

val sub : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction

val mad : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
            src2:PtxVal.value -> src3:PtxVal.value -> instruction 
            
val setp_le : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction 
                 
val setp_lt : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction 
            
val setp_gt : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction
  
val setp_ge : PtxType.ty -> dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction
                
                
val setp_eq : PtxType.ty ->  dest:PtxVal.value -> src1:PtxVal.value -> 
                 src2:PtxVal.value -> instruction

val slct : PtxType.ty -> PtxType.ty -> dest:PtxVal.value -> left:PtxVal.value -> 
             right:PtxVal.value -> switch:PtxVal.value -> instruction 
            
val cvt : 
      t1:PtxType.ty -> t2:PtxType.ty -> dest:PtxVal.value -> src:PtxVal.value ->
        instruction 
        
        
val ld : ?offset:int -> space:PtxVal.ptx_space -> PtxType.ty -> 
           PtxVal.value -> PtxVal.value -> instruction
          
val ld_global : ?offset:int -> PtxType.ty -> 
                  PtxVal.value -> PtxVal.value -> instruction
           
val ld_param : ?offset:int -> PtxType.ty -> 
           PtxVal.value -> PtxVal.value -> instruction
            
val ld_shared : ?offset:int -> PtxType.ty -> 
           PtxVal.value -> PtxVal.value -> instruction
          
val ld_local : ?offset:int -> PtxType.ty -> 
           PtxVal.value -> PtxVal.value -> instruction
           
val ld_const : ?offset:int -> PtxType.ty -> 
           PtxVal.value -> PtxVal.value -> instruction
           

val st : ?offset:int -> space:PtxVal.ptx_space -> ty:PtxType.ty -> 
           dest:PtxVal.value ->  src:PtxVal.value -> instruction
          
val st_global : ?offset:int -> ty:PtxType.ty -> 
           dest:PtxVal.value ->  src:PtxVal.value -> instruction
           
val st_shared : ?offset:int -> ty:PtxType.ty -> 
           dest:PtxVal.value ->  src:PtxVal.value -> instruction
          
val st_local : ?offset:int -> ty:PtxType.ty -> 
           dest:PtxVal.value ->  src:PtxVal.value -> instruction


val tex : geom -> PtxType.ty -> 
            PtxVal.value -> PtxVal.value -> PtxVal.value -> PtxVal.value -> 
            PtxVal.value -> PtxVal.value -> PtxVal.value -> PtxVal.value ->
            PtxVal.value -> instruction 
          
val bar : instruction 

val selp : dest:PtxVal.value -> ifTrue:PtxVal.value -> ifFalse:PtxVal.value -> 
           cond:PtxVal.value -> instruction 

val bra : label -> instruction 

val mov : ?ty:PtxType.ty -> PtxVal.value -> PtxVal.value -> instruction

type special_reg_3d = { x:PtxVal.value; y:PtxVal.value; z:PtxVal.value }

val tid : special_reg_3d 
val ntid : special_reg_3d 
val ctaid : special_reg_3d 
val nctaid : special_reg_3d 
