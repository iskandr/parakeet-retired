open Base
open Ptx
open PtxType 
open PtxVal 
open PtxHelpers

exception InvalidGpuOp of gpu_op

let rec simplify codegen instr  =
  match instr.op with
  | VirtualUnop (Exp,F32) ->
      let tmp1 = codegen#fresh_reg F32 in
      let tmp2 = codegen#fresh_reg F32 in 
      [(mov F32 tmp1 (FloatConst 1.44269504));
       (mul F32 tmp2 instr.args.(1) tmp1);
       {instr with op   = Unop (Ex2 Approx,F32);
                   args = [|instr.args.(0); tmp2|]}]
  | VirtualUnop (Exp, t) -> 
      let downCvt = codegen#fresh_reg F32 in 
      let tmp1 = codegen#fresh_reg F32 in 
      let tmp2 = codegen#fresh_reg F32 in
      let f32Result = codegen#fresh_reg F32 in 
      [(cvt ~t1:F32 ~t2:t ~dest:downCvt ~src:instr.args.(1)); 
       (mov t tmp1 (FloatConst 1.44269504));
       (mul t tmp2 downCvt tmp1);
       {instr with op   = Unop (Ex2 Approx,F32);
                   args = [|f32Result; tmp2|]
       };
       (cvt ~t1:t ~t2:F32 ~dest:instr.args.(0) ~src:f32Result)
      ] 
  | VirtualUnop (Ln,F32) ->
      let tmp1 = codegen#fresh_reg F32 in
      let tmp2 = codegen#fresh_reg F32 in 
      [(mov F32 tmp1 (FloatConst 0.693147181));
       {instr with op = Unop (Lg2 Approx, F32);
                   args = [|tmp2; instr.args.(1)|]};
       (mul F32 instr.args.(0) tmp2 tmp1)]
  | VirtualUnop(Ln, t) -> 
      let downCvt = codegen#fresh_reg F32 in 
      let tmp1 = codegen#fresh_reg F32 in 
      let tmp2 = codegen#fresh_reg F32 in
      let f32Result = codegen#fresh_reg F32 in 
      [(cvt ~t1:F32 ~t2:t ~dest:downCvt ~src:instr.args.(1)); 
       (mov F32 tmp1 (FloatConst 0.693147181));
       {instr with op = Unop (Lg2 Approx, F32);
                   args = [|tmp2; downCvt|]};
       (mul F32 f32Result tmp2 tmp1);
       (cvt ~t1:t ~t2:F32 ~dest:instr.args.(0) ~src:f32Result)
      ]  
  | _ -> [instr]
