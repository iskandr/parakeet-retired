(* state of the code generator includes:
    -- instructions thus far emitted
    -- free registers of all GPU types
    -- total number of registers of all GPU types
        (used for generating allocations)

    This module also has code generation helper functions
*)

open Base
open Ptx
open PtxHelpers


         

let rec reg_prefix = function 
  | U8 -> "r_char_"
  | S8 -> "r_schar_"
  | U16 -> "rh"
  | S16 -> "rsh"
  | U32 -> "r"
  | S32 -> "rs"
  | U64 -> "rl"
  | S64 -> "rsl"
  | F16 -> "rfh"
  | F32 -> "rf"
  | F64 -> "rd"
  | Pred -> "p"
  | B8 -> "r_byte"
  | B16 -> "rbh"
  | B32 -> "rb"
  | B64 -> "rbl"
  | V2 t -> (reg_prefix t) ^ "_v2" 
  | V4 t -> (reg_prefix t) ^ "_v4"


(* generate single register var_decl *)
let mk_reg_decl ~t = {
  t = t; 
  space = REG; 
  array_size = None; 
  init_val=None 
}

let concat name id = name ^ (string_of_int id)
let reg_name gpuT id = concat (reg_prefix gpuT) id

let data_param_name id = concat "param" id
let shape_param_name id = concat "shape" id

  
let initialNumRegs = 29

 

class ptx_codegen  = object (self)  

  (* maintain a symbols table, so we don't have to use strings  
     when referring to parameters or registers 
  *)   
  val symbols : (int, string) Hashtbl.t = Hashtbl.create 31 
  val revSymbols : (string, int) Hashtbl.t = Hashtbl.create 31
   
  val mutable symCounter = 0 
  method private get_sym_id name = 
    if Hashtbl.mem revSymbols name then Hashtbl.find revSymbols name 
    else (
      symCounter <- symCounter + 1; 
      Hashtbl.add symbols symCounter name;
      Hashtbl.add revSymbols name symCounter;  
      symCounter
    )

    
  val maxRegs : (PtxType.ty, int) Hashtbl.t =  Hashtbl.create initialNumRegs
  
  val instructions : instruction DynArray.t = DynArray.create ()

  val allocations: (Ptx.symid, Ptx.var_decl) Hashtbl.t = Hashtbl.create 17
  method private add_alloc id newAlloc = Hashtbl.add allocations id newAlloc 

  val parameters: (Ptx.symid * PtxType.ty) DynArray.t = DynArray.create ();
  method private add_param_decl id newParam = 
    DynArray.add parameters (id, newParam) 

  
  (* cache the register objects corresponding to the values 
     and possibly the shapes and lengths of arguments 
  *) 
  val dataRegs : (ID.t, PtxVal.value) Hashtbl.t = 
     Hashtbl.create initialNumRegs
   
  val shapeRegs : (ID.t, PtxVal.value) Hashtbl.t = 
     Hashtbl.create initialNumRegs
    
  val dynTypes : (ID.t, DynType.t) Hashtbl.t = 
     Hashtbl.create initialNumRegs 
    
  val mutable labelCounter = 0
  method fresh_label = 
    labelCounter <- labelCounter + 1; 
    let name = "$Label" ^ (Int.to_string labelCounter) in 
    self#get_sym_id name 
         
  val mutable numArgs = 0
  
  
     
 
  (* PUBLIC METHODS *) 
    
  method finalize_kernel =
    PtxTidy.cleanup_kernel instructions allocations; 
    { 
      params = DynArray.to_array parameters; 
      code = DynArray.to_array instructions; 
      decls = PMap.of_enum (Hashtbl.enum allocations);
      symbols = PMap.of_enum (Hashtbl.enum symbols); 
    }
  
       
      
method emit newInstructions = 
    DynArray.append (DynArray.of_list newInstructions) instructions
    
  method fresh_reg gpuT =
    let currMax = match Hashtbl.find_option maxRegs gpuT with 
      | Some max -> max
      | None -> 0 
    in 
    Hashtbl.replace maxRegs gpuT (currMax + 1);
    let id = self#get_sym_id (reg_name gpuT currMax) in  
    self#add_alloc id {
      t = gpuT; 
      space = REG; 
      array_size = None; 
      init_val= None 
    };
    Reg id
  
  method init_reg  gpuT initVal =
    let reg = self#fresh_reg gpuT in 
    self#emit [mov gpuT reg initVal]; 
    reg
     
  (** returns a list of register ids for requested type **)
  method fresh_regs gpuT count =
    let regs = Array.create count (Reg 0) in 
    for i = 0 to count - 1 do 
      regs.(i) <- self#fresh_reg gpuT
    done;
    regs 

  (* if variable is a scalar then allocates one new register and returns
     it. if it's a vector then allocate a register pair (data/shape) and
     return the data register. If the vector is actually a slice through
     a shared vector, then we don't need a shape pointer.  
  *) 
  method declare_local id dynT =
    let ptxT = PtxType.of_dyn_type dynT in 
    let dataReg = self#fresh_reg ptxT in 
    Hashtbl.add dataRegs id dataReg;
    Hashtbl.add dynTypes id dynT; 
    if not $ DynType.is_scalar dynT then  (
      Hashtbl.add shapeRegs id (self#fresh_reg PtxType.ptrT)
    );
    dataReg
    
 method declare_shared_slice id = 
   let dataReg = self#fresh_reg PtxType.ptrT in
   Hashtbl.add dataRegs id dataReg;
   dataReg
  
  method declare_arg id dynT = 
    Hashtbl.add dynTypes id dynT; 
    let num = numArgs in
    numArgs <- num + 1;
    let paramId = self#get_sym_id (data_param_name num) in  
    let dataParam =  Param paramId in
    let gpuStorageT = PtxType.storage_of_dyn_type dynT in
    self#add_param_decl paramId gpuStorageT; 
    let storageReg = self#fresh_reg gpuStorageT in 
    self#emit [ld_param gpuStorageT storageReg dataParam];
    let gpuT = PtxType.of_dyn_type dynT in
    let dataReg =  
        (* 
           if storage and register types aren't the same, 
           then we need conversion code (for types like bool, u8, etc..)
        *) 
        if gpuT = gpuStorageT then storageReg 
        else self#convert ~destType:gpuT ~srcType:gpuStorageT ~srcReg:storageReg 
    in 
    Hashtbl.add dataRegs id dataReg;
    (* vectors need an additional param for their shape *) 
    if not $ DynType.is_scalar dynT then  (
      let shapeParamId = self#get_sym_id (shape_param_name num) in 
      let shapeParam = Param shapeParamId in 
      let shapeReg = self#fresh_reg PtxType.ptrT in 
      self#add_param_decl shapeParamId PtxType.ptrT ; 
      self#emit [ld_param PtxType.ptrT shapeReg shapeParam];
      Hashtbl.add shapeRegs id shapeReg  
    );
    dataReg
   
  method get_data_reg id = match Hashtbl.find_option dataRegs id with 
    | Some reg -> reg
    | None -> failwith $ "[GpuCodegen] Unregistered arg " ^ (dump id)
   
  method get_shape_reg id = match Hashtbl.find_option shapeRegs id with
    | Some reg -> reg
    | None -> failwith $ "[GpuCodegen] Unregistered shape arg " ^ (dump id)

    
  method get_dyn_type id = match Hashtbl.find_option dynTypes id with 
    | Some t -> t 
    | None -> failwith $ "[GpuCodegen] No type registered for " ^ (ID.to_str id) 
   
  (* convert from srcType to destType, with special consideration 
     given to Predicates. This would be nice to put in PtxHelpers, 
     but must live here because it requires allocation of a register 
  *) 
  method convert ~destType ~srcType ~srcReg =
  if srcType = destType then srcReg 
  else 
    let destReg = self#fresh_reg destType in begin 
    if destType = Pred then self#emit [setp_eq srcType destReg srcReg (int 0)]
    else if srcType = Pred then 
      self#emit [selp destType destReg (int 1) (int 0) srcReg]
    else if PtxType.is_int srcType && PtxType.is_float destType then
      self#emit [round Ptx.RoundNearest (cvt destType srcType destReg srcReg)]
    else self#emit [cvt destType srcType destReg srcReg]
    ;
    destReg
  end     
  

  method run_rewrite_pass f  = 
    let n = DynArray.length instructions in
    let newCode = DynArray.make n in
    let insert = (DynArray.add newCode) in
    for i = 0 to n -1 do
      let instr = DynArray.get instructions i in
      let rewriteList = f self instr in
      List.iter insert rewriteList
    done;
    DynArray.clear instructions;
    DynArray.append newCode instructions

end
