

open Ptx
open PtxType

 
let _ = Random.self_init () 

type num = IntVal of Int64.t | FloatVal of float 

type gpuval = {num:num; gpu_type:PtxType.ty} 
type vec3 = {x:int; y:int; z:int} 

let ones_vec = {x = 1; y = 1; z=1}

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
        if signed then Int64.sub (Int64.shift_left 1L (bits - 1)) 1L 
        else Int64.sub (Int64.shift_left 1L bits) 1L 
      in 
      IntVal (rand_int lower upper)
    else if PtxType.is_float t then FloatVal (rand_float (-1.0) 1.0)
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
  | NumCtaId dim -> get gridSize dim 
  | ProcessorId -> 0 
  | MaxProcessors -> 16
  | GridId -> 0 
  | Clock -> 0 

let eval_value gridSize blockSize blockIdx threadIdx registers = function 
  | Reg {id=id; ptx_type=t} -> 
      if Hashtbl.mem registers id then Hashtbl.find registers id
      else rand_val t 
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

let cvt t = function 
  | IntVal i -> 
      if PtxType.is_float t then FloatVal (Int64.to_float i)
      else if PtxType.is_int t then IntVal i 
      else assert false
  | FloatVal f -> 
      if PtxType.is_float t then FloatVal f 
      else if PtxType.is_int t then IntVal (Int64.of_float f) 
      else assert false  
  
let find_labels code = 
  let labels = Hashtbl.create 17 in 
  for i = 0 to Array.length code - 1 do 
    match code.(i).label with 
      | None -> ()
      | Some lbl -> Hashtbl.add labels lbl i 
  done; 
  labels 
  
(* TODO: make a hashtbl of label positions *) 
let emulate_with_random_data   
    (gridSize : vec3) 
    (blockSize : vec3)
    (kernel : Ptx.kernel) = 
  let blockIdx = rand_vec3 gridSize in 
  let threadIdx = rand_vec3 blockSize in 
  let code = kernel.code in
  let labels = find_labels code in 
  let registers =  Hashtbl.create 127 in
  let step = ref 0 in 
  let maxsteps = 2000 in
  let programCounter = ref 0 in
  let maxCounter = Array.length code  in  
  while !programCounter < maxCounter  && !step < maxsteps do 
    let instr = code.(!programCounter) in 
    (* this might get reset to something else by a branch instruction *)  
    programCounter := !programCounter + 1;
    let args = instr.args in  
    let eval_arg = 
      eval_value gridSize blockSize blockIdx threadIdx registers
    in 
    match instr.op  with
    | Bra lbl -> programCounter := Hashtbl.find labels lbl 
    | Bar _ 
    | Comment _ -> ()
    | Exit -> programCounter := maxCounter
    | _ -> ()
    (*
    | Cvt (targetT, _) -> 
        let cvtVal = cvt targetT (eval_arg args.(1)) in  
    
    | Unop (unop, t) -> 
    | Binop (binop, t) -> 
    | Mad (bits, t) ->   
    | Mad24 (bits, t) -> 
    | FloatMad (bits, t) ->    
    | Ld (space, t) ->     
    | LdOffset (space, t, offset) ->  
    | Selp of PtxType.ty 
    | Slct of PtxType.ty * PtxType.ty  
    | St of space * PtxType.ty 
    | StOffset of space * PtxType.ty * int 
    | Mov of PtxType.ty 
    *)
  done;
  Ok 
  