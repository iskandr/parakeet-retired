

open Ptx
open PtxType

 
let _ = Random.self_init () 

type num = IntVal of Int64.t | FloatVal of float 

type gpuval = {num:num; gpu_type:PtxType.ty} 
type vec3 = {x:int; y:int; z:int} 

let rand_vec3 limits = { 
  x = Random.int limits.x;
  y = Random.int limits.y; 
  z = Random.int limits.z;
}   

let rand_int lower upper = 
  let range = Int64.sub upper lower in 
  let randint = Random.int64 range in 
  Int64.sub randint lower

let rand_float lower upper = 
  let range = upper -. lower in 
  let randfloat = Random.float range in 
  randfloat -. lower

let rand_val t = 
  let bits = min (PtxType.nbytes t * 8) 63 in
  let num = 
    if PtxType.is_int t then
      let signed = PtxType.is_signed t in 
      let lower = 
        if signed then 
          Int64.neg (Int64.shift_left 1L (bits-1))
        else 0L
      in  
      let upper = 
        if signed then 
          Int64.minus_one (Int64.shift_left 1L (bits - 1)) 
        else Int64.minus_one (Int64.shift_left bits) 
      in 
      IntVal (rand_int lower upper)
   else if PtxType.is_float then FloatVal (rand_float -1.0 1.0)
   else failwith "random val of this type not implemented"
  in {num=num; gpu_type= t}
  
(*and instruction = { 
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
}*)

 
let get v = function X -> v.x | Y -> v.y | Z -> v.z 

let eval_special gridSize blockSize blockIdx threadIdx = function   
  | ThreadId dim -> get threadIdx dim 
  | NumThreadId dim -> get blockSize dim 
  | ThreadLane -> 0 
  | WarpId -> 0 
  | CtaId dim -> get blockIdx dim 
  | NumCtaId -> get gridSize dim 
  | ProcessorId -> 0 
  | MaxProcessors -> 16
  | GridId -> 0 
  | Clock -> 0 

let eval_value gridSize blockSize blockIdx threadIdx registers = function 
  | Reg {id=id} -> Hashtbl.find registers id 
  | Param {ptx_type=t} -> 
      (* never actually look at params, just generate random data *) 
      rand_val t
  | IntConst i64 -> {num = IntVal i64; gpu_type = S64 } 
  | FloatConst f -> {num = FloatVal f; gpu_type = F64 } 
  | Special r -> 
     let i = 
      Int64.of_int (eval_special gridSize blockSize blockIdx threadIdx r)
     in {num=IntVal i; gpu_type=PtxVal.type_of_special_register r}  

type status = Ok | Fail of int   
 
  
let emulate_with_random_data   
    (kernel : Ptx.kernel)
    (gridSize : vec3) 
    (blockSize : vec3) = 
  let blockIdx = rand_vec3 gridSize in 
  let threadIdx = rand_vec3 blockSize in 
  let code = kernel.code in
  let registers =  Hashtbl.create 127 in
  let finished = ref false in 
  let step = ref 0 in 
  let maxsteps = 2000 in
  let programCounter = ref 0 in
  while not !finished && !step < maxsteps do 
    let instr = code.(!programCounter) in 
    (* this might get reset to something else by a branch instruction *)  
    programCounter := !programCounter + 1; 
    let argVals = 
      Array.map 
        (eval_value gridSize blockSize blockIdx threadIdx registers)
        instr 
    in 
    match instr.op  with  
    | Unop (unop, t) -> 
    | Binop (binop, t) -> 
    | Mad (bits, t) ->   
    | Mad24 (bits, t) -> 
    | FloatMad (bits, t) ->    
    | Cvt (targetT, sourceT) -> 
    | Ld (space, t) ->     
    | LdOffset (space, t, offset) ->  
    | Selp of PtxType.ty 
    | Slct of PtxType.ty * PtxType.ty  
    | St of space * PtxType.ty 
    | StOffset of space * PtxType.ty * int 
    | Mov of PtxType.ty 
    | Bra of label
    | Comment _ -> Ok  
    | Exit -> Ok  
    | Bar of int  
  done
  