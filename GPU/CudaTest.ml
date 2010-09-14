open PTX
open PTX_Helpers
open Cuda
open LibPQ

let sample_k = {
	entry = "add2Kernel";
	
	(*
	.reg          .u16 %rh<4>;\n\
  .reg          .u32 %r<12>;\n\
  .reg          .u64 %rd<9>;\n\
  .reg          .pred %p1;\n\
	.param        .u64 output_array;\n\
  .param        .u64 x_array;\n\
  .param        .u64 y_array;\n\
  .param        .s32 n;\n\
 *)
	allocations = 
		(repeatReg ~name:"rh" ~count:4 ~t:U16) @
		(repeatReg ~name:"r" ~count:12 ~t:U32) @
		(repeatReg ~name:"rd" ~count:9 ~t:U64) @ 
		[(mkReg ~name:"p1" ~t:Pred);
	   (mkParam ~name:"output_array" ~t:U64); 
		 (mkParam ~name:"x_array" ~t:U64); 
		 (mkParam ~name:"y_array" ~t:U64); 
		 (mkParam ~name:"n" ~t:S32)];
		
  code = [
	  (* mov.u16       %rh1, %ntid.x;\n\ *)
		(mkop (Mov U16) ~target:(Reg "rh1") ~src:(Special (ThreadsPerDim X))); 
		(*   mov.u16       %rh2, %ctaid.x;\n\ *)
		(mkop (Mov U16) ~target:(Reg "rh2") ~src:(Special (CTA_Id X)));
		(* mul.wide.u16  %r1, %rh2, %rh1;\n\ *)
		(mkop2 (Mul(AllBits,U16)) ~target:(Reg "r1") 
					~src1:(Reg "rh2") ~src2:(Reg "rh1")); 
		(*mov.u16       %rh3, %nctaid.x;\n\ *)
		(mkop (Mov U16) ~target:(Reg "rh3") ~src:(Special (CTA_PerDim X)));
		(*  mul.wide.u16  %r2, %rh3, %rh1;\n\ *)
		(mkop2 (Mul(AllBits,U16)) ~target:(Reg "r2") 
                ~src1:(Reg "rh3") ~src2:(Reg "rh1"));
		(* cvt.u32.u16   %r3, %ctaid.y;\n\ *)
		(mkop (Cvt(U32,U16)) ~target:(Reg "r3") ~src:(Special (CTA_Id Y)));  
		(*  mul.lo.u32    %r4, %r3, %r2; *)
		(mkop2 (Mul(LowBits, U32)) ~target:(Reg "r4") 
                ~src1:(Reg "r3") ~src2:(Reg "r2"));
		(* add.u32       %r5, %r1, %r4;\n\ *)
		(mkop2 (Binop(Add,U32)) ~target:(Reg "r5") 
                ~src1:(Reg "r1") ~src2:(Reg "r4"));
		(* cvt.u32.u16   %r6, %tid.x;\n\ *)
		(mkop (Cvt(U32, U16)) ~target:(Reg "r6") ~src:(Special (ThreadId X))); 	
  	(* add.u32       %r7, %r6, %r5;\n\ *) 
		(mkop2 (Binop(Add, U32)) ~target:(Reg "r7") 
                ~src1:(Reg "r6") ~src2:(Reg "r5")); 
		(* ld.param.s32  %r8, [n]; *) 
		(mkop (Ld(PARAM, S32)) ~target:(Reg "r8") ~src:(Deref (Param "n"))); 
		(* setp.le.s32   %p1, %r8, %r7;\n\ *) 
		(mkop2 (Setp(LE, S32)) (Reg "p1") (Reg "r8") (Reg "r7"));
		(* @%p1 bra      $Out_of_bounds;\n\ *)
		(mkop0 ~pred:(Reg "p1") (Bra "$Out_of_bounds")); 
		(* cvt.u64.s32   %rd1, %r7;  *) 
		(mkop (Cvt(U64,S32)) (Reg "rd1") (Reg "r7")); 
		(* mul.lo.u64    %rd2, %rd1, 4; *)
		(mkop2 (Mul(LowBits,U64)) (Reg "rd2") (Reg "rd1") (Const 4));
  	(* ld.param.u64  %rd3, [x_array]; *)
		(mkop (Ld(PARAM, U64)) (Reg "rd3") (Deref (Param "x_array"))); 
  	(* add.u64       %rd4, %rd3, %rd2; *)
		(mkop2 (Binop (Add,U64)) (Reg "rd4") (Reg "rd3") (Reg "rd2"));  
  	(* ld.global.s32 %r9, [%rd4+0];  *)
		(mkop (Ld(GLOBAL, S32)) (Reg "r9") (Deref (Reg "rd4")));  
  	(* ld.param.u64  %rd5, [y_array]; *)
		(mkop (Ld(PARAM, U64)) (Reg "rd5") (Deref (Param "y_array")));   
  	(* add.u64       %rd6, %rd5, %rd2; *)
		(mkop2 (Binop (Add, U64)) (Reg "rd6") (Reg "rd5") (Reg "rd2"));  
  	(* ld.global.s32 %r10, [%rd6+0]; *)
		(mkop (Ld(GLOBAL, S32)) (Reg "r10") (Deref (Reg "rd6")));  
  	(* add.s32       %r11, %r9, %r10; *)
		(mkop2 (Binop (Add, S32)) (Reg "r11") (Reg "r9") (Reg "r10"));  
  	(* ld.param.u64  %rd7, [output_array]; *)
		(mkop (Ld (PARAM, U64)) (Reg "rd7") (Deref (Param "output_array")));  
  	(* add.u64       %rd8, %rd7, %rd2; *)
		(mkop2 (Binop (Add, U64)) (Reg "rd8") (Reg "rd7") (Reg "rd2"));  
  	(* st.global.s32 [%rd8+0], %r11; *)
		(mkop (St(GLOBAL,S32)) (Deref (Reg "rd8")) (Reg "r11"));  
 		(* $Out_of_bounds: *) 
  	(* exit; *) 
		(mkop0 ~label:"$Out_of_bounds" Exit);
  
	 ]
}


let _ = cuda_init()