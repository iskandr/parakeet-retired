open Base

open PtxType
open PtxVal 
open Ptx
 
let prim_to_ptx_binop op t = match op,t with
  | Prim.Add,_ -> Binop(Add,t)
  | Prim.Sub,_ -> Binop(Sub,t)
  | Prim.Mult, F32
  | Prim.Mult, F64 -> Binop(FloatMul, t)
  | Prim.Mult, _ -> Binop(IntMul Low, t)
  | Prim.Div, F32 -> Binop (FloatDiv Approx, F32)
  | Prim.Div, F64 -> Binop (FloatDiv RN, F64)
  | Prim.Div, _ -> Binop (IntDiv, t)
  | Prim.Lt, _ -> Binop(Setp LT,t)
  | Prim.Gt, _ -> Binop(Setp GT,t)
  | Prim.Gte, _ -> Binop(Setp GE,t)
  | Prim.Lte, _ -> Binop(Setp LE,t)
  | Prim.Eq, _ -> Binop(Setp EQ,t)
  | Prim.Neq, _ -> Binop(Setp NE,t)
  | p, _-> failwith $
    Printf.sprintf "[prim->ptx] binop %s not implemented for type %s: "
     (Prim.scalar_op_to_str p)
     (PtxType.to_str t)

let prim_to_ptx_unop  unop t = match unop,t with 
  | Prim.Abs, _ -> Unop(Abs,t)
  | Prim.Neg,_ -> Unop(Neg,t) 
  | Prim.Not,_ -> Unop(Not,t) 
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

let int64 x = IntConst x 
let int x = IntConst (Int64.of_int x)
let float x = FloatConst x 

let num_to_ptx_const = function 
  | PQNum.Char c -> int $  Char.code c
  | PQNum.Int i -> int i
  | PQNum.Int32 i -> int64 (Int64.of_int32 i)
  | PQNum.Int64 i -> int64 i
  | PQNum.Float32 f
  | PQNum.Float64 f -> float f
  | PQNum.Bool b -> int64 $ if b then 1L else 0L

let mkop op args = {
   op=op; args=args;  label=None; pred=NoGuard;
   sat = false; ftz = false; rounding=None
}

(* add label or predicate to instruction *)
let pred p instr = {instr with pred = IfTrue p}
let pred_not p instr = {instr with pred = IfFalse p} 
let label l instr = {instr with label = Some l}
let round mode instr = { instr with rounding = Some mode } 

let op0 op = mkop op [||]
let op1 op x = mkop op [|x|] 
let op2 op x y = mkop op [|x;y|]
let op3 op x y z = mkop op [|x;y;z|]
let op4 op x y z w = mkop op [|x;y;z;w|]

let unop op ty ~dest ~src = mkop (Unop (op,ty)) [|dest; src|]
let virtual_unop op ty ~dest ~src = mkop (VirtualUnop (op,ty)) [|dest; src|]
let binop op ty ~dest ~src1 ~src2 = mkop (Binop (op,ty)) [|dest; src1; src2|]
let virtual_binop op ty dest src1 src2 = 
  mkop (VirtualBinop(op,ty)) [|dest; src1; src2|]


let comment ~txt = op0 $ Comment txt

(* different ways to multiply depending on type, so check the *)
(* big case function above *)
let mul ty ~dest ~src1 ~src2 =
  mkop (prim_to_ptx_binop Prim.Mult ty) [|dest; src1; src2|]
let div ty ~dest ~src1 ~src2 =
  mkop (prim_to_ptx_binop Prim.Div ty) [|dest; src1; src2|]

let mul_wide = binop (IntMul Wide)
let mul_lo = binop (IntMul Low)
let mul_hi = binop (IntMul High)
let add = binop Add
let sub = binop Sub

(* TODO: fix this to actually generate MAD instructions, *)
(* currently just multiplies. Also need to generate diff instructions *)
(* for floats and ints-- and need to fail for small types like u8, pred, etc *) 
let mad ty ~dest ~src1 ~src2 ~src3 = mul ty ~dest ~src1 ~src2

let setp_le = binop (Setp LE)
let setp_lt = binop (Setp LT)
let setp_gt = binop (Setp GT)
let setp_ge = binop (Setp GE)
let setp_eq = binop (Setp EQ)

let slct tDest tSwitch ~dest ~left ~right ~switch = 
  mkop (Slct (tDest, tSwitch)) [|dest; left; right; switch|]  

let cvt ~t1 ~t2 ~dest ~src = 
  let instr = mkop (Cvt(t1,t2)) [|dest; src|] in 
  if PtxType.nbytes t1 >= PtxType.nbytes t2 then instr
  else if PtxType.is_float t1 then round Ptx.RoundNearest instr 
  else if PtxType.is_int t1 then round Ptx.RoundNearest_Int instr 
  else failwith 
    (Printf.sprintf "[ptx_cvt] uncertain how to convert %s into %s" 
      (PtxType.to_str t2) (PtxType.to_str t1))   

let ld ?offset ~space ty dest src = 
  match offset with 
    | None -> mkop (Ld(space,ty)) [|dest; src|]
    | Some offset -> mkop (LdOffset(space,ty,offset)) [|dest; src|]

let ld_global ?offset =  ld ?offset ~space:GLOBAL 
let ld_param ?offset = ld ?offset ~space:PARAM 
let ld_shared ?offset = ld ?offset ~space:SHARED
let ld_local ?offset = ld ?offset ~space:LOCAL 
let ld_const ?offset = ld ?offset ~space:CONST 

let st ?offset ~space ~ty ~dest ~src = match offset with 
  | None -> mkop (St(space,ty)) [|dest;src|]
  | Some offset -> mkop (StOffset(space,ty,offset)) [|dest;src|]

let st_global ?offset = st ?offset ~space:GLOBAL 
let st_shared ?offset = st ?offset ~space:SHARED
let st_local ?offset = st ?offset ~space:LOCAL 

let bar = mkop (Bar 0) [||]

let selp ty ~dest ~ifTrue ~ifFalse ~cond  = 
  mkop (Selp ty)[|dest;ifTrue;ifFalse;cond|] 
let bra label = op0 $ Bra label 
let mov ty dest src = mkop (Mov ty) [|dest; src|] 



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
