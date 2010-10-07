open Base
open Printf

include PtxVal
include PtxType 

type label = int 
and space = REG | SREG | CONST | GLOBAL | LOCAL | PARAM | SHARED | SURF | TEX
and gpu_comp =
    EQ | NE | LT | LE | GT | GE | LO | LS | HI | HS
  | EQU | NEU | LTU | LEU | GTU | GEU | NUM | NAN 
and gpu_bits = Wide | Low | High 
and gpu_rounding_mode = 
  | RoundNearest_Int
  | RoundZero_Int
  | RoundNegInf_Int
  | RoundPosInf_Int
  | RoundNearest
  | RoundZero
  | RoundNegInf
  | RoundPosInf
and gpu_float_mode =
  | Approx
  | Full
  | RN (* same as RoundNearest, but used for DIV operator *)
and gpu_unop =
  Abs | Cnot   | Neg | Not
  | Rsqrt of gpu_float_mode
  | Cos of gpu_float_mode
  | Ex2 of gpu_float_mode
  | Lg2 of gpu_float_mode
  | Sin of gpu_float_mode
  | Sqrt of gpu_float_mode
  | Rcp of gpu_float_mode
(*   Tex | Vote | Trap |   Atom  | Bar | Brkpt  | Call *)
and gpu_binop =
      Add | And | Max | Min | Or
    | Rem | Ret | Sad | Set | Shl | Shr | Sub | Xor
    | Setp of gpu_comp
    | FloatDiv of gpu_float_mode
    | FloatMul
    | IntDiv
    | IntMul of gpu_bits
    | Mul24 of gpu_bits
and gpu_virtual_unop = Exp | Ln
and gpu_virtual_binop =  Pow

and gpu_op =
  | Unop of gpu_unop * PtxType.ty
  | Binop of gpu_binop * PtxType.ty
  | VirtualUnop of gpu_virtual_unop * PtxType.ty
  | VirtualBinop of gpu_virtual_binop * PtxType.ty
  | Mad of gpu_bits * PtxType.ty  
  | Mad24 of gpu_bits * PtxType.ty
  | FloatMad of PtxType.ty   
  | Cvt of PtxType.ty * PtxType.ty
  | Ld of space * PtxType.ty * int 
  | Selp of PtxType.ty 
  | Slct of PtxType.ty * PtxType.ty  
  | St of space * PtxType.ty * int 
  | Mov of PtxType.ty 
  | Bra of label
  | Comment of string 
  | Exit 
  | Bar of int 
 
and var_decl = {
  t: PtxType.ty; 
	space: space;
	array_size: int option;
	init_val: int option;
}
(* instructions can be conditional on a predicate, 
   its negation, or not at all
*) 
and guard = IfTrue of PtxVal.value | IfFalse of PtxVal.value | NoGuard  
and instruction = { 
  op: gpu_op; 
  sat : bool;
  ftz : bool;
  rounding : gpu_rounding_mode option; 
  args : PtxVal.value array; 
	label : label option; 
	pred : guard
}
and kernel = {
  params: (symid * PtxType.ty) array;
  decls: (symid, var_decl) PMap.t; 
  symbols : (symid, string) PMap.t; 
	code: instruction array
}
and gpu_compute_capability = SM_10 | SM_11 | SM_13 | SM_20
and ptx_module = {
  kernels : (string, kernel) PMap.t;
  compute_capability: gpu_compute_capability;
}



let is_float_rounding_mode = function 
  | RoundNearest | RoundZero | RoundNegInf | RoundPosInf -> true 
  | _ -> false

let is_int_rounding_mode = function 
  | RoundNearest_Int |  RoundZero_Int
  | RoundNegInf_Int | RoundPosInf_Int -> true
  | _ -> false


let rec ptx_module_to_str ptxModule =
  let approx_numlines k =  
    Enum.count (PMap.enum k.decls) +
    Array.length k.code + 
    Array.length k.params  in
  let kernel_lengths = PMap.map approx_numlines ptxModule.kernels in
  let numlines = PMap.foldi (fun _ len acc -> len + acc) kernel_lengths 0 in
  let b = Buffer.create (40 * numlines) in
  (*let append = Buffer.add_string buffer in *)
  Buffer.add_string b ".version 1.4 \n";
  bprintf b ".target %s \n" 
     (compute_capability_to_str ptxModule.compute_capability);  
  PMap.iter (add_kernel_to_buffer b) ptxModule.kernels;
  Buffer.contents b
and add_kernel_to_buffer b name k = 
  bprintf b ".entry %s \n" name;
  add_kernel_params_to_buffer b k.symbols k.params;
  Buffer.add_string b "{\n";
  add_kernel_decls_to_buffer b k.symbols k.decls; 
  Buffer.add_string b "\n";
  add_kernel_body_to_buffer b k.symbols k.code;  
  Buffer.add_string b "}\n"

and kernel_to_str k name = 
  let b = Buffer.create 100 in 
  add_kernel_to_buffer b name k;
  Buffer.contents b  
and add_kernel_params_to_buffer b  symbols params = 
  let n = Array.length params in 
  if n > 0 then begin 
    let paramStrings = 
      Array.map (fun (id,ty) -> param_to_str symbols id ty) params in 
    Buffer.add_string b "(";
    for i = 0 to n - 2  do 
        bprintf b "%s, " paramStrings.(i)
    done;
    Buffer.add_string b paramStrings.(n-1);
    Buffer.add_string b ")\n";
  end 
and param_to_str symbols id ty = 
  sprintf ".param .%s %s" (PtxType.to_str ty) (PMap.find id symbols)
and add_kernel_decls_to_buffer buffer symbols decls = 
    PMap.iter 
      (fun id decl -> 
        bprintf buffer ".%s\t .%s %%%s;\n" 
         (gpu_space_to_str decl.space)
         (PtxType.to_str decl.t)
         (PMap.find id symbols)        
      ) 
     decls

and add_kernel_body_to_buffer b symbols code =
  Array.iter 
    (fun instr -> 
      add_instruction_to_buffer b symbols instr; 
      Buffer.add_string b "\n") 
    code

and add_instruction_to_buffer b symbols instr = 
  (match instr.label with 
	  | Some l -> bprintf b "%s:\n" (gpu_label_to_str (PMap.find l symbols))
    | None -> ()
  );  
	(match instr.pred with 
    | IfTrue p -> bprintf b "@%s " (PtxVal.to_str symbols p)
    | IfFalse p -> bprintf b "@!%s " (PtxVal.to_str symbols p)
    | NoGuard -> ()
  ); 
  Buffer.add_string b (gpu_op_name instr.op);
  (match instr.rounding with 
    | Some mode -> bprintf b ".%s" (gpu_rounding_mode_to_str mode)
    | None -> ()
  );
  if instr.sat then Buffer.add_string b ".sat";
  if instr.ftz then Buffer.add_string b ".ftz";
  gpu_op_args_to_buffer b symbols instr.op instr.args

and gpu_op_name = function 
  | Unop (unop, _) ->  gpu_unop_to_str unop 
  | Binop (binop, _) -> gpu_binop_to_str binop
  | VirtualUnop (unop, _) -> sprintf "virtual.%s" $ gpu_virtual_unop_to_str unop
  | VirtualBinop (binop, t) ->
     sprintf "virtual.%s" $ gpu_virtual_binop_to_str binop
  | Mad(bits,_) -> sprintf "mad.%s" $ gpu_bits_to_str bits
  | Mad24(bits,_) -> sprintf "mad24.%s" $ gpu_bits_to_str bits
  | FloatMad t -> sprintf "mad"
  | Cvt _ -> "cvt"
  | Selp t -> "selp"
  | Slct (t1,t2) ->  "slct"
  | Ld _ -> "ld" 
  | St _ -> "st" 
  | Mov _ -> "mov"				
  | Bra _  -> "bra" 
  | Exit -> "exit"
  | Bar d -> "bar.sync " ^ (string_of_int d)
  | Comment _ -> ""
(* print everything after the op name and rounding/ftz/sat modifiers *) 
and gpu_op_args_to_buffer b symbols op args = match op with 
  | Bra label -> bprintf b " %s;" (PMap.find label symbols) 
  | Exit -> Buffer.add_string b ";"
  | Comment str -> bprintf b "/* %s */" str
  | Mov t -> 
      bprintf b ".%s\t %s, %s;" 
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0)) 
        (PtxVal.to_str symbols args.(1))
  | Ld(space, t, 0) -> 
      bprintf b ".%s.%s\t %s, [%s];" 
       (gpu_space_to_str space) 
       (PtxType.to_str t)
       (PtxVal.to_str symbols args.(0)) 
       (PtxVal.to_str symbols args.(1))
  | Ld(space, t, offset)-> 
      bprintf b ".%s.%s\t %s, [%s+%d];" 
        (gpu_space_to_str space) 
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0)) 
        (PtxVal.to_str symbols args.(1))
        offset
  | St(space, t, 0) ->  
      bprintf b ".%s.%s\t [%s], %s;" 
        (gpu_space_to_str space) 
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0))
        (PtxVal.to_str symbols args.(1))
  | St(space, t, offset) ->
      bprintf b ".%s.%s\t [%s+%d], %s;" 
        (gpu_space_to_str space) 
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0))
        offset 
        (PtxVal.to_str symbols args.(1))
  | VirtualUnop (_, t) 
  | Unop (_, t) -> 
      bprintf b ".%s\t %s, %s;" 
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0))
        (PtxVal.to_str symbols args.(1))
                
  | VirtualBinop (_, t)
  | Binop (_, t) -> 
      bprintf b ".%s\t %s, %s, %s;" 
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0))
        (PtxVal.to_str symbols args.(1))
        (PtxVal.to_str symbols args.(2))
(* MAD instructions include a bit-width, which is printed as part 
   of their names in gpu_op_name 
  *)
  | Selp t  
  | FloatMad t
  | Mad(_,t)  
  | Mad24(_,t) ->
      bprintf b ".%s\t %s, %s, %s, %s;"
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0))
        (PtxVal.to_str symbols args.(1))
        (PtxVal.to_str symbols args.(2))
        (PtxVal.to_str symbols args.(3))
  | Slct (t1,t2) -> 
      bprintf b ".%s.%s\t %s, %s, %s, %s;" 
        (PtxType.to_str t1)
        (PtxType.to_str t2)
        (PtxVal.to_str symbols args.(0))
        (PtxVal.to_str symbols args.(1))
        (PtxVal.to_str symbols args.(2))
        (PtxVal.to_str symbols args.(3))

  | Cvt (t1,t2) -> 
      bprintf b ".%s.%s\t %s, %s;" 
        (PtxType.to_str t1) 
        (PtxType.to_str t2)
        (PtxVal.to_str symbols args.(0))
        (PtxVal.to_str symbols args.(1))
         
  | _ -> bprintf b ";"
and gpu_bits_to_str = function 
	| Wide -> "wide"
	| Low -> "lo"
	| High -> "hi"
					
and gpu_space_to_str = function 
	| REG -> "reg"
	| SREG -> "sreg" 
	| CONST -> "const" 
	| GLOBAL -> "global"
	| LOCAL -> "local"
	| PARAM -> "param"
	| SHARED -> "shared"
	| SURF -> "surf"
	| TEX -> "tex" 
and gpu_comp_to_str = function 	
	| EQ -> "eq"
	| NE -> "ne"
	| LT -> "lt"
	| LE -> "le"
	| GT -> "gt"
	| GE -> "ge"
	| LO -> "lo"
	| LS -> "ls"
	| HI -> "hi"
	| HS -> "hs"
	| EQU -> "equ"
	| NEU -> "neu"
	| LTU -> "ltu"
	| LEU -> "leu"
	| GTU -> "gtu"
	| GEU -> "geu"
	| NUM -> "num"
	| NAN -> "nan"
and gpu_unop_to_str = function 
	| Abs -> "abs"
 	| Cnot -> "cnot"
	| Neg -> "neg"
  | Not -> "not"
  | Cos mode -> sprintf "cos.%s" $ gpu_float_mode_to_str mode  
  | Sin mode -> sprintf "sin.%s" $ gpu_float_mode_to_str mode 
	| Lg2 mode -> sprintf "lg2.%s" $ gpu_float_mode_to_str mode 
  | Ex2 mode -> sprintf "ex2.%s" $ gpu_float_mode_to_str mode 
	| Rsqrt mode -> sprintf "rsqrt.%s" $ gpu_float_mode_to_str mode
  | Sqrt mode -> sprintf "sqrt.%s" $ gpu_float_mode_to_str mode
  | Rcp mode -> sprintf "rcp.%s" $ gpu_float_mode_to_str mode
  
and gpu_binop_to_str = function 
	| Add -> "add"
	| And -> "and"
	| Max -> "max"
	| Min -> "min"
	| Or -> "or"

	| Rem -> "rem"
	| Ret -> "ret"
	| Sad -> "sad"

  | FloatDiv mode ->  sprintf "div.%s" (gpu_float_mode_to_str mode)
  | FloatMul ->  "mul" 
  
  | IntDiv -> "div"
  | IntMul bits -> sprintf "mul.%s" (gpu_bits_to_str bits)
  
  | Setp comp -> sprintf "setp.%s" (gpu_comp_to_str comp)
  | Mul24 bits -> sprintf "mul24.%s" (gpu_bits_to_str bits)

	| Set -> "set"
	| Shl -> "shl"
	| Shr -> "shr"

  | Xor -> "xor"
  | Sub -> "sub"
	(*
  | Tex -> "tex"
	| Atom -> "atom"
  | Bar -> "bar"
  | Brkpt -> "brkpt"
  | Call -> "call"
        | Vote -> "vote"
  | Trap -> "trap"
	*)
and gpu_virtual_binop_to_str = function 
  | Pow -> "pow"
and gpu_virtual_unop_to_str = function 
  | Ln -> "ln"
  | Exp -> "exp"
and gpu_label_to_str label = label
and gpu_rounding_mode_to_str = function 
  | RoundNearest_Int -> "rni"
  | RoundZero_Int -> "rzi"
  | RoundNegInf_Int -> "rmi"
  | RoundPosInf_Int -> "rpi"
  | RoundNearest -> "rn"
  | RoundZero -> "rz"
  | RoundNegInf ->  "rm"
  | RoundPosInf -> "rp"
and gpu_float_mode_to_str = function 
  | Full -> "full"
  | Approx -> "approx"
  | RN -> "rn"
and compute_capability_to_str = function
  | SM_10 -> "sm_10" 
  | SM_11 -> "sm_11"
  | SM_13 -> "sm_13"
  | SM_20 -> "sm_20"
