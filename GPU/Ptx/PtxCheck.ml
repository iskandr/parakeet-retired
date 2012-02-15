open Base 
open Ptx
open PtxType

module Interval = struct 
 
end

open Interval 

type vec3 = {x:int; y:int; z:int} 

let ones_vec = {x = 1; y = 1; z=1}
 
let get v = function X -> v.x | Y -> v.y | Z -> v.z 

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
  
let find_labels code = 
  let labels = Hashtbl.create 17 in 
  for i = 0 to Array.length code - 1 do 
    match code.(i).label with 
      | None -> ()
      | Some lbl -> Hashtbl.add labels lbl i 
  done; 
  labels 

(* interpreter evaluates all possible paths through a program 
   by keeping a stack of continuations 
*) 
type store = (PtxVal.value, range) PMap.t 
type continuation = int * store 

let initialize_store gridSize blockSize =
  PMap.of_enum $ List.enum [
    Special (ThreadId X), int_range 0 (blockSize.x - 1);
    Special (ThreadId Y), int_range 0 (blockSize.y - 1); 
    Special (ThreadId Z), int_range 0 (blockSize.z - 1); 
    Special (CtaId X), int_range 0 (gridSize.x - 1); 
    Special (CtaId Y), int_range 0 (gridSize.y - 1); 
    Special (CtaId Z), int_range 0 (gridSize.z - 1); 
    Special ThreadLanem, int_const 0; 
    Special (NumThreadId X), int_const blockSize.x;          
    Special (NumThreadId Y), int_const blockSize.y; 
    Special (NumThreadId Z), int_const blockSize.z; 
    Special (NumCtaId X), int_const gridSize.x; 
    Special (NumCtaId Y), int_const gridSize.y;
    Special (NumCtaId Z), int_const gridSize.z;
    Special GridId, int_range 0 15; 
    Special Clock, int_range 0 int_inf; 
    Special MaxProcessors, int_const 16;
    Special ProcessorId, int_range 0 15;
  ]
(* TODO: make a hashtbl of label positions *) 
let emulate_with_random_data   
    (gridSize : vec3) 
    (blockSize : vec3)
    (kernel : Ptx.kernel) = 
  let code = kernel.code in
  let labels = find_labels code in 
  let step = ref 0 in 
  let maxsteps = 2000 in
  let continuations = Stack.create () in
  let store = initialize_store gridSize blockSize in  
  Stack.push (0, store) continuations;
  while not $ Stack.is_empty continuations && !step < maxsteps do
    let programCounter, store = Stack.pop continuations in  
    let instr = code.(programCounter) in 
    (* this might get reset to something else by a branch instruction *)  
    let nextCounter = ref (programCounter + 1);
    let args = instr.args in 
    let eval_arg = eval_value store in 
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
