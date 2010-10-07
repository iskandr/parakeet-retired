
open Base 
open Ptx
open PtxType

module Range = struct 
  type range =
  | IntRange of Int64.t * Int64.to_int
  | FloatRange of float * float  

  let int_range lower upper = IntRange (Int64.of_int lower, Int64.of_int upper)
  let int_const x = int_range x x 
  
  let float_range lower upper = FloatRange(lower,upper) 
  let float_const x = float_range x x 
  
  let true_range = IntRange (1L, 1L)
  let false_range = IntRange (0L, 0L)

  let any_true = List.fold_left (||) false
  let all_true = List.fold_left (&&) true 

  let float_neg_inf = -1000000000000.0
  let float_inf = 1000000000000.0
  let unknown_float = FloatRange (float_neg_inf, float_inf)

  let int_inf = Int64.shift_left 1L 30
  let int_neg_inf = Int64.sub 0L int_inf 
  let unknown_int = IntRange(int_inf, int_neg_inf)


  let range_of_bools bs = 
    if all_true bs then IntRange (1L, 1L)
    else if any_true bs then IntRange (0L, 1L)
    else IntRange (0L, 0L)

  let cmp (intCmp, floatCmp) n1 n2 = 
    match n1, n2 with 
    | IntRange (x,y), IntRange(a,b) -> 
        val_of_bools [intCmp x a; intCmp x b; intCmp y a; intCmp y b]
    | FloatRange (x,y), FloatRange(a,b) -> 
        val_of_bools [floatCmp x a; floatCmp x b; floatCmp y a; floatCmp y b]            
    | _ -> failwith "[ptx_check] mixed number types in cmp"

  let lt = cmp ((<), (<)) 
  let eq = cmp ((=), (=))

  let range_of_ints (ints :Int64.t list) = 
    let minval = List.fold_left min int_neg_inf ints in 
    let maxval = List.fold_left max int_inf ints in 
    IntRange (max minval int_neg_inf, min maxval int_inf)

  let range_of_floats (floats: float list) = 
    let minval = List.fold_left min float_neg_inf floats in 
    let maxval = List.fold_left max float_inf floats in 
    FloatRange (max minval float_neg_inf, min maxval float_inf)


  let binop (intFn, floatFn) n1 n2 = 
    match n1, n2 with 
    | IntRange(x,y), IntRange(a,b) -> 
      range_of_ints [intFn x a; intFn a b; intFn y a; intFn y b]
    | FloatRange (x,y), FloatRange(a,b) -> 
      range_of_floats [floatFn x a; floatFn x b; floatFn y a; floatFn y b]
    | _ -> failwith "[ptx_check] mixed number types in binop"

  let unop (intFn, floatFn) = function 
    | IntRange (x, y) -> range_of_ints [intFn x; intFn y]
    | FloatRange (x,y) -> range_of_float [floatFn x; floatFn y]  
  
  (* Doesn't handle overflow when converting from U64 to U8 *)
  let cvt t = function  
  | IntRange(x,y) -> 
      if PtxType.is_float t then FloatRange (Int64.to_float x, Int64.to_float y)
      else if PtxType.is_int t then IntRange(x,y)  
      else assert false
  | FloatRange(x,y) -> 
      if PtxType.is_float t then FloatRange(x,y)  
      else if PtxType.is_int t then IntVal (Int64.of_float x, Int64.of_float y) 
      else assert false  

end

open Range 

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

  