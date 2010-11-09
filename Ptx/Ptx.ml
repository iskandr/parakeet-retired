open Base
open Printf

include PtxVal
include PtxType 

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
  Abs | Cnot   | Neg | Not
  | Rsqrt of ptx_float_mode
  | Cos of ptx_float_mode
  | Ex2 of ptx_float_mode
  | Lg2 of ptx_float_mode
  | Sin of ptx_float_mode
  | Sqrt of ptx_float_mode
  | Rcp of ptx_float_mode
(*   Tex | Vote | Trap |   Atom  | Bar | Brkpt  | Call *)
and ptx_binop =
      Add | And | Max | Min | Or
    | Rem | Ret | Sad | Set | Shl | Shr | Sub | Xor
    | Setp of ptx_comp
    | FloatDiv of ptx_float_mode
    | FloatMul
    | IntDiv
    | IntMul of ptx_bits
    | Mul24 of ptx_bits
and ptx_virtual_unop = Exp | Ln
and ptx_virtual_binop =  Pow
and geom = Tex1D | Tex2D | Tex3D 
and ptx_op =
  | Unop of ptx_unop * PtxType.ty
  | Binop of ptx_binop * PtxType.ty
  | VirtualUnop of ptx_virtual_unop * PtxType.ty
  | VirtualBinop of ptx_virtual_binop * PtxType.ty
  | Mad of ptx_bits * PtxType.ty  
  | Mad24 of ptx_bits * PtxType.ty
  | FloatMad of PtxType.ty   
  | Cvt of PtxType.ty * PtxType.ty
  | Ld of ptx_space * PtxType.ty * int 
  | Selp of PtxType.ty 
  | Slct of PtxType.ty * PtxType.ty  
  | St of ptx_space * PtxType.ty * int 
  | Mov of PtxType.ty 
  | Bra of label
  | Comment of string 
  | Exit 
  | Bar of int 
  | Tex of geom * ty
 
and var_decl = {
  t: PtxType.ty; 
	decl_space: ptx_space;
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
  params: (symid * PtxType.ty) array;
  decls: (symid, var_decl) Hashtbl.t; 
  symbols : (symid, string) Hashtbl.t; 
	code: instruction array; 
  textures: symid array;  
}

and ptx_compute_capability = SM_10 | SM_11 | SM_13 | SM_20
and ptx_module = {
  kernels : (string, kernel) PMap.t;
  compute_capability: ptx_compute_capability;
}


let module_from_named_kernels (kernels : (string * kernel) list) =
  (* TODO: rename the textures for global consistency *) 
  { 
    kernels = PMap.of_enum (List.enum kernels);   
    compute_capability = SM_13
  } 


let is_float_rounding_mode = function 
  | RoundNearest | RoundZero | RoundNegInf | RoundPosInf -> true 
  | _ -> false

let is_int_rounding_mode = function 
  | RoundNearest_Int |  RoundZero_Int
  | RoundNegInf_Int | RoundPosInf_Int -> true
  | _ -> false



(*********************************************************
                TO_STRING FUNCTIONS 
 *********************************************************)

let rec ptx_module_to_str ptxModule =
  let approx_numlines k =  
    Hashtbl.length k.decls +
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
  sprintf ".param .%s %s" (PtxType.to_str ty) (Hashtbl.find symbols id)
and add_kernel_decls_to_buffer buffer symbols decls =
    let add_decl id decl =
      let name = Hashtbl.find symbols id in
      let spaceStr = ptx_space_to_str decl.decl_space in   
      match decl.decl_space, decl.array_size with 
        | SHARED, Some size ->
          let bytesPerElt = PtxType.nbytes decl.t in  
          let totalBytes =  bytesPerElt * size in
          bprintf buffer ".shared\t .align %d .b8 %s[%d];\n"
            bytesPerElt (* is this the correct alignment for small types? *) 
            name
            totalBytes 
        | SHARED, None -> failwith "can't declare shared var without size"
        | _, Some _ -> failwith "declaring non-shared arrays not implemented"
        | space, None ->   
            bprintf buffer ".%s\t .%s %%%s;\n" 
              spaceStr
              (PtxType.to_str decl.t)
              name 
    in 
    Hashtbl.iter add_decl decls

and add_kernel_body_to_buffer b symbols code =
  Array.iter 
    (fun instr -> 
      add_instruction_to_buffer b symbols instr; 
      Buffer.add_string b "\n") 
    code

and add_instruction_to_buffer b symbols instr = 
  (match instr.label with 
	  | Some l -> bprintf b "%s:\n" (ptx_label_to_str (Hashtbl.find symbols l))
    | None -> ()
  );  
	(match instr.pred with 
    | IfTrue p -> bprintf b "@%s " (PtxVal.to_str symbols p)
    | IfFalse p -> bprintf b "@!%s " (PtxVal.to_str symbols p)
    | NoGuard -> ()
  ); 
  Buffer.add_string b (ptx_op_name instr.op);
  (match instr.rounding with 
    | Some mode -> bprintf b ".%s" (ptx_rounding_mode_to_str mode)
    | None -> ()
  );
  if instr.sat then Buffer.add_string b ".sat";
  if instr.ftz then Buffer.add_string b ".ftz";
  ptx_op_args_to_buffer b symbols instr.op instr.args

and ptx_op_name = function 
  | Unop (unop, _) ->  ptx_unop_to_str unop 
  | Binop (binop, _) -> ptx_binop_to_str binop
  | VirtualUnop (unop, _) -> sprintf "virtual.%s" $ ptx_virtual_unop_to_str unop
  | VirtualBinop (binop, t) ->
     sprintf "virtual.%s" $ ptx_virtual_binop_to_str binop
  | Mad(bits,_) -> sprintf "mad.%s" $ ptx_bits_to_str bits
  | Mad24(bits,_) -> sprintf "mad24.%s" $ ptx_bits_to_str bits
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
and ptx_op_args_to_buffer b symbols op args = match op with 
  | Bra label -> bprintf b " %s;" (Hashtbl.find symbols label) 
  | Exit -> Buffer.add_string b ";"
  | Comment str -> bprintf b "\t /* %s */" str
  | Mov t -> 
      bprintf b ".%s\t %s, %s;" 
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0)) 
        (PtxVal.to_str symbols args.(1))
  | Ld(space, t, 0) -> 
      bprintf b ".%s.%s\t %s, [%s];" 
       (ptx_space_to_str space) 
       (PtxType.to_str t)
       (PtxVal.to_str symbols args.(0)) 
       (PtxVal.to_str symbols args.(1))
  | Ld(space, t, offset)-> 
      bprintf b ".%s.%s\t %s, [%s+%d];" 
        (ptx_space_to_str space) 
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0)) 
        (PtxVal.to_str symbols args.(1))
        offset
  | St(space, t, 0) ->  
      bprintf b ".%s.%s\t [%s], %s;" 
        (ptx_space_to_str space) 
        (PtxType.to_str t)
        (PtxVal.to_str symbols args.(0))
        (PtxVal.to_str symbols args.(1))
  | St(space, t, offset) ->
      bprintf b ".%s.%s\t [%s+%d], %s;" 
        (ptx_space_to_str space) 
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
   of their names in ptx_op_name 
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
and ptx_bits_to_str = function 
	| Wide -> "wide"
	| Low -> "lo"
	| High -> "hi"
					

and ptx_comp_to_str = function 	
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
and ptx_unop_to_str = function 
	| Abs -> "abs"
 	| Cnot -> "cnot"
	| Neg -> "neg"
  | Not -> "not"
  | Cos mode -> sprintf "cos.%s" $ ptx_float_mode_to_str mode  
  | Sin mode -> sprintf "sin.%s" $ ptx_float_mode_to_str mode 
	| Lg2 mode -> sprintf "lg2.%s" $ ptx_float_mode_to_str mode 
  | Ex2 mode -> sprintf "ex2.%s" $ ptx_float_mode_to_str mode 
	| Rsqrt mode -> sprintf "rsqrt.%s" $ ptx_float_mode_to_str mode
  | Sqrt mode -> sprintf "sqrt.%s" $ ptx_float_mode_to_str mode
  | Rcp mode -> sprintf "rcp.%s" $ ptx_float_mode_to_str mode
  
and ptx_binop_to_str = function 
	| Add -> "add"
	| And -> "and"
	| Max -> "max"
	| Min -> "min"
	| Or -> "or"

	| Rem -> "rem"
	| Ret -> "ret"
	| Sad -> "sad"

  | FloatDiv mode ->  sprintf "div.%s" (ptx_float_mode_to_str mode)
  | FloatMul ->  "mul" 
  
  | IntDiv -> "div"
  | IntMul bits -> sprintf "mul.%s" (ptx_bits_to_str bits)
  
  | Setp comp -> sprintf "setp.%s" (ptx_comp_to_str comp)
  | Mul24 bits -> sprintf "mul24.%s" (ptx_bits_to_str bits)

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
and ptx_virtual_binop_to_str = function 
  | Pow -> "pow"
and ptx_virtual_unop_to_str = function 
  | Ln -> "ln"
  | Exp -> "exp"
and ptx_label_to_str label = label
and ptx_rounding_mode_to_str = function 
  | RoundNearest_Int -> "rni"
  | RoundZero_Int -> "rzi"
  | RoundNegInf_Int -> "rmi"
  | RoundPosInf_Int -> "rpi"
  | RoundNearest -> "rn"
  | RoundZero -> "rz"
  | RoundNegInf ->  "rm"
  | RoundPosInf -> "rp"
and ptx_float_mode_to_str = function 
  | Full -> "full"
  | Approx -> "approx"
  | RN -> "rn"
and compute_capability_to_str = function
  | SM_10 -> "sm_10" 
  | SM_11 -> "sm_11"
  | SM_13 -> "sm_13"
  | SM_20 -> "sm_20"






(****************************************************************
                    HELPER/SHORTCUT FUNCTIONS 
 ****************************************************************)

let prim_to_ptx_binop op t = match op,t with
  | Prim.Add,_ -> Binop(Add,t)
  | Prim.Sub,_ -> Binop(Sub,t)
  | Prim.Mult, F32
  | Prim.Mult, F64 -> Binop(FloatMul, t)
  | Prim.Mult, _ -> Binop(IntMul Low, t)
  | Prim.Div, F32 -> Binop (FloatDiv Approx, F32)
  | Prim.Div, F64 -> Binop (FloatDiv RN, F64)
  | Prim.Div, _ -> Binop (IntDiv, t)
  | Prim.Lt, t when PtxType.is_unsigned t ->  Binop(Setp LO,t)
  | Prim.Lt, _ ->  Binop(Setp LT,t)
  | Prim.Gt, t when PtxType.is_unsigned t -> Binop(Setp HI,t)
  | Prim.Gt, _ -> Binop(Setp GT,t)
  | Prim.Gte, t when PtxType.is_unsigned t -> Binop(Setp HS,t)
  | Prim.Gte, _ -> Binop(Setp GE,t)
  | Prim.Lte, t when PtxType.is_unsigned t-> Binop(Setp LS,t)
  | Prim.Lte, _ -> Binop(Setp LE,t)
  | Prim.Eq, _ -> Binop(Setp EQ,t)
  | Prim.Neq, _ -> Binop(Setp NE,t)
  | Prim.And, Pred -> Binop(And,Pred)
  | Prim.Or, Pred -> Binop(Or,Pred)
  | p, _-> failwith $
    Printf.sprintf "[prim->ptx] binop %s not implemented for type %s: "
     (Prim.scalar_op_to_str p)
     (PtxType.to_str t)

let prim_to_ptx_unop  unop t = match unop,t with 
  | Prim.Abs, _ -> Unop(Abs,t)
  | Prim.Neg,_ -> Unop(Neg,t) 
  | Prim.Not, Pred -> Unop(Not,Pred) 
  | Prim.Sqrt,F64 ->  Unop(Sqrt RN,F64)
  | Prim.Sqrt, F32 -> Unop(Sqrt Approx,F32) 
  | Prim.Reciprocal, F32 -> Unop(Rcp Approx,F32)
  | Prim.Reciprocal, F64 -> Unop(Rcp RN,F64)
  | Prim.Exp2,F32 -> Unop(Ex2 Approx,F32)
  | Prim.Lg2,F32 -> Unop(Lg2 Approx,F32) 
  | Prim.Exp,_ -> VirtualUnop(Exp,t) 
  | Prim.Ln,_ -> VirtualUnop(Ln,t)
  | u,_ -> failwith $
    Printf.sprintf "[prim->ptx] unop %s not implemented for type %s: "
     (Prim.scalar_op_to_str u) (PtxType.to_str t)

(* translate op without knowing its arity *) 
let prim_to_ptx_op op t = 
  if Prim.is_unop op then prim_to_ptx_unop op t 
  else if Prim.is_binop op then prim_to_ptx_binop op t 
  else failwith "[ptx_helpers] dont't know how to translate op"

let int64 x = IntConst x 
let int x = IntConst (Int64.of_int x)
let float x = FloatConst x 

let num_to_ptx_const = function 
  | PQNum.Char c -> int $  Char.code c
  | PQNum.Int32 i -> int64 (Int64.of_int32 i)
  | PQNum.Int64 i -> int64 i
  | PQNum.Float32 f
  | PQNum.Float64 f -> float f
  | PQNum.Bool b -> int64 $ if b then 1L else 0L

let mkop op args = {
   op=op; args=Array.of_list args;  label=None; pred=NoGuard;
   sat = false; ftz = false; rounding=None
}

(* add label or predicate to instruction *)
let pred p instr = {instr with pred = IfTrue p}
let pred_not p instr = {instr with pred = IfFalse p} 
let label l instr = {instr with label = Some l}
let round mode instr = { instr with rounding = Some mode } 

let op0 op = mkop op []
let op1 op x = mkop op [x] 
let op2 op x y = mkop op [x;y]
let op3 op x y z = mkop op [x;y;z]
let op4 op x y z w = mkop op [x;y;z;w]

let unop op ty ~dest ~src = mkop (Unop (op,ty)) [dest; src]
let virtual_unop op ty ~dest ~src = mkop (VirtualUnop (op,ty)) [dest; src]
let binop op ty ~dest ~src1 ~src2 = mkop (Binop (op,ty)) [dest; src1; src2]
let virtual_binop op ty dest src1 src2 = 
  mkop (VirtualBinop(op,ty)) [dest; src1; src2]


let comment ~txt = op0 $ Comment txt

(* different ways to multiply depending on type, so check the *)
(* big case function above *)
let mul ty ~dest ~src1 ~src2 =
  mkop (prim_to_ptx_binop Prim.Mult ty) [dest; src1; src2]
let div ty ~dest ~src1 ~src2 =
  mkop (prim_to_ptx_binop Prim.Div ty) [dest; src1; src2]

let mul_wide = binop (IntMul Wide)
let mul_lo = binop (IntMul Low)
let mul_hi = binop (IntMul High)
let add = binop Add
let sub = binop Sub

(* TODO: fix this to actually generate MAD instructions, *)
(* currently just multiplies. Also need to generate diff instructions *)
(* for floats and ints-- and need to fail for small types like u8, pred, etc *) 
let mad ty ~dest ~src1 ~src2 ~src3 = failwith "MAD not implemented"

let setp_le ty = 
  if PtxType.is_signed ty then 
    binop (Setp LE) ty
  else 
    binop (Setp LS) ty 
    
let setp_lt ty = 
  if PtxType.is_signed ty then 
    binop (Setp LT) ty
  else
    binop (Setp LO) ty 
 
let setp_gt ty =
  if PtxType.is_signed ty then
    binop (Setp GT) ty
  else 
    binop (Setp HI) ty 
    
let setp_ge ty = 
  if PtxType.is_signed ty then 
    binop (Setp GE) ty 
  else 
    binop (Setp HS) ty 

let setp_eq = binop (Setp EQ)

let slct tDest tSwitch ~dest ~left ~right ~switch = 
  mkop (Slct (tDest, tSwitch)) [dest; left; right; switch]  

let cvt ~t1 ~t2 ~dest ~src = 
  let instr = mkop (Cvt(t1,t2)) [dest; src] in 
  (* if the conversion is safe or if it is a down-conversion 
     between integer types then there is no possibility of rounding 
  *)
  if PtxType.nbytes t1 >= PtxType.nbytes t2 
     ||  PtxType.is_int t1 && PtxType.is_int t2 then instr
  (* float to int conversion *) 
  else if PtxType.is_int t1 && PtxType.is_float t2 then 
    round RoundNearest_Int instr
  (* float to float *)  
  else if PtxType.is_float t1 && PtxType.is_float t2 then 
    round RoundNearest instr 
  else failwith 
    (Printf.sprintf "[ptx_cvt] uncertain how to convert %s into %s" 
      (PtxType.to_str t2) (PtxType.to_str t1))   

let ld ?offset ~space ty dest src = 
  let offset' = match offset with None -> 0 | Some offset -> offset in 
  mkop (Ld(space,ty,offset')) [dest; src]

let ld_global ?offset =  ld ?offset ~space:GLOBAL 
let ld_param ?offset = ld ?offset ~space:PARAM 
let ld_shared ?offset = ld ?offset ~space:SHARED
let ld_local ?offset = ld ?offset ~space:LOCAL 
let ld_const ?offset = ld ?offset ~space:CONST 

let st ?offset ~space ~ty ~dest ~src = 
  let offset' = match offset with None -> 0 | Some offset -> offset in 
  mkop (St(space,ty,offset')) [dest;src]

let st_global ?offset = st ?offset ~space:GLOBAL 
let st_shared ?offset = st ?offset ~space:SHARED
let st_local ?offset = st ?offset ~space:LOCAL 

let bar = mkop (Bar 0) []

let selp (*?ty*) ~dest ~ifTrue ~ifFalse ~cond  =
  (*let ty = match ty with | None -> PtxVal.type_of_var dest | Some t -> t in*)
  let ty = PtxVal.type_of_var dest in  
  mkop (Selp ty)[dest;ifTrue;ifFalse;cond] 
let bra label = op0 $ Bra label 

let mov ?ty dest src = 
  let ty = match ty with | None -> PtxVal.type_of_var dest | Some t -> t in
  mkop (Mov ty) [dest; src] 

type special_reg_3d = { x:PtxVal.value; y:PtxVal.value; z:PtxVal.value }

let tid = { 
  x=Special (ThreadId X); 
  y=Special (ThreadId Y); 
  z=Special (ThreadId Z)
}

let ntid = { 
  x=Special (NumThreadId X); 
  y=Special (NumThreadId Y); 
  z=Special (NumThreadId Z)
}

let ctaid = {
  x = Special (CtaId X);
  y = Special (CtaId Y);
  z = Special (CtaId Z)
}

let nctaid = {
  x = Special (NumCtaId X);
  y = Special (NumCtaId Y);
  z = Special (NumCtaId Z)
}


