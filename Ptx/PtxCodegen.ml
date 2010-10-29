(* state of the code generator includes:
    -- instructions thus far emitted
    -- free registers of all GPU types
    -- total number of registers of all GPU types
        (used for generating allocations)

    This module also has code generation helper functions
*)

open Base
open Ptx
open PtxVal 
open PtxHelpers

let initialNumRegs = 29 
  
class ptx_codegen  = object (self)  
  
  (* SYMBOL TABLE *) 
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
    
  (* GENERATE FRESH NAMES FOR EXTERNAL ARGUMENTS *) 
  val mutable numArgs = 0
  method private fresh_arg_id = 
    let num = numArgs in
    numArgs <- num + 1;
    self#get_sym_id ("__param" ^ (string_of_int num))

  (* GENERATE FRESH NAMES FOR SHARED VECTORS *) 
  val mutable numShared = 0 
  
  (* keep track of fixed dimensions of shared arrays pointed to by a register *) 
  val sharedDims : (PtxVal.symid, int array) Hashtbl.t = Hashtbl.create 17
  
  method private fresh_shared_id  = 
    let num = numShared in
    numShared <- num + 1;
    self#get_sym_id ("__shared" ^(string_of_int num)) 


  method is_shared_ptr = function 
    | PtxVal.Sym {id=id} -> Hashtbl.mem sharedDims id
    | _ -> false   
  
  method get_shared_dims = function 
    | PtxVal.Sym{id=id} -> 
        assert (Hashtbl.mem sharedDims id); 
        Hashtbl.find sharedDims id
    | other -> 
      failwith $ 
      "can't get shared dimesions for PTX value " ^
      (PtxVal.to_str symbols other) 
  

  (* GENERATE FRESH LABELS *) 
  val mutable labelCounter = 0
  method fresh_label = 
    labelCounter <- labelCounter + 1; 
    let name = "$Label" ^ (Int.to_string labelCounter) in 
    self#get_sym_id name 

  (* PTX CODE *) 
  val instructions : instruction DynArray.t = DynArray.create ()
  method emit newInstructions = 
    DynArray.append (DynArray.of_list newInstructions) instructions
  
  (* VARIABLE DECLARATIONS *) 
  val allocations: (Ptx.symid, Ptx.var_decl) Hashtbl.t = 
    Hashtbl.create initialNumRegs
  method private add_alloc id newAlloc = 
    Hashtbl.add allocations id newAlloc 
  
  (* FUNCTION PARAMETERS *) 
  val parameters: (Ptx.symid * PtxType.ty) DynArray.t = DynArray.create ()
  method private add_param_decl id newParam = 
    DynArray.add parameters (id, newParam) 

  (* number of registers currently allocated for every type *) 
  val maxRegs : (PtxType.ty, int) Hashtbl.t =  Hashtbl.create initialNumRegs
  
  (* Mapping from Imp identifiers to their corresponding registers *)  
  val dataRegs : (ID.t, PtxVal.value) Hashtbl.t = Hashtbl.create initialNumRegs
  method imp_reg id = match Hashtbl.find_option dataRegs id with 
    | Some reg -> reg
    | None -> failwith $ "[PtxCodegen] Unregistered arg " ^ (ID.to_str id)

  val dynTypes : (ID.t, DynType.t) Hashtbl.t = Hashtbl.create initialNumRegs
  method imp_dyn_type id = match Hashtbl.find_option dynTypes id with 
    | Some t -> t 
    | None -> failwith $ "[PtxCodegen] No type registered for " ^ (ID.to_str id) 
  
  (* Any register pointing to a global array should also have an accompanying 
     register pointing to a shape vector.  
  *)
  val shapeRegs : (PtxVal.symid, PtxVal.value) Hashtbl.t = 
    Hashtbl.create initialNumRegs
    
  (* get the shape register associated with an array register *) 
  method get_shape_reg ( arrayPtr : PtxVal.value) = 
    match Hashtbl.find_option shapeRegs (PtxVal.get_id arrayPtr ) with
    | Some reg -> reg
    | None -> 
        failwith $ 
          "[PtxCodegen] Unregistered shape arg " ^ 
          (PtxVal.to_str symbols arrayPtr)
   
  (* global array ranks -- the expected length of each array's shape vector *)
  val globalArrayRanks : (PtxVal.symid, int) Hashtbl.t = 
    Hashtbl.create initialNumRegs
    
  method get_global_array_rank ( arrayPtr : PtxVal.value ) = 
    match Hashtbl.find_option globalArrayRanks (PtxVal.get_id arrayPtr) with
      | Some r -> r
      | None -> failwith $ 
          "[PtxCodegen] Unable to find rank: PTX value " ^ 
          (PtxVal.to_str symbols arrayPtr) ^ " not registered as a global array" 
  
  method is_global_array_ptr = function 
    | PtxVal.Sym {id=id} -> Hashtbl.mem globalArrayRanks id 
    | _ -> false 
  
  (* works for both shared and global vectors *) 
  method get_array_rank (ptr: PtxVal.value) = 
    if self#is_shared_ptr ptr then Array.length (self#get_shared_dims ptr)
    else if self#is_global_array_ptr ptr then self#get_global_array_rank ptr 
    else failwith "[ptx_codegen] can't get array rank of non-array register" 
  
  (* get a register which points to the shape vector attached to the 
     argument "ptrReg" which contains the address of some array's data. 
  *) 
  method private fresh_shape_reg ptrReg rank =
    let shapeReg = self#fresh_reg PtxType.ptrT in
    let ptrId = PtxVal.get_id ptrReg in 
    Hashtbl.add shapeRegs ptrId shapeReg;
    Hashtbl.add globalArrayRanks ptrId rank; 
    shapeReg  
  
  method fresh_reg gpuT =
    let rec reg_prefix = function 
    | U8 -> "r_char_" | U16 -> "rh" | U32 -> "r" | U64 -> "rl"
    | S8 -> "r_schar_" | S16 -> "rsh" | S32 -> "rs" | S64 -> "rsl"
    | F16 -> "rfh" | F32 -> "rf" | F64 -> "rd"
    | B8 -> "r_byte" | B16 -> "rbh" | B32 -> "rb" | B64 -> "rbl"
    | Pred -> "p"
    | V2 t -> (reg_prefix t) ^ "_v2" 
    | V4 t -> (reg_prefix t) ^ "_v4"
    in 
    let reg_name = fun gpuT id -> (reg_prefix gpuT) ^ (string_of_int id) in 
    let currMax = match Hashtbl.find_option maxRegs gpuT with 
      | Some max -> max
      | None -> 0 
    in 
    Hashtbl.replace maxRegs gpuT (currMax + 1);
    let id = self#get_sym_id (reg_name gpuT currMax) in  
    self#add_alloc id {
      t = gpuT; 
      decl_space = REG; 
      array_size = None; 
      init_val= None 
    };
    Sym {id=id; ptx_type=gpuT; space=REG}
  
 (** returns a list of register ids for requested type **)
  method fresh_regs gpuT count =
    Array.init count (fun _ -> self#fresh_reg gpuT)

  
  (* if variable is a scalar then allocates one new register and returns
     it. if it's a vector then allocate a register pair (data/shape) and
     return the data register. If the vector is actually a slice through
     a shared vector, then we don't need a shape pointer.  
  *) 
  method declare_local impId dynT =
    let ptxT = PtxType.of_dyn_type dynT in 
    let dataReg = self#fresh_reg ptxT in 
    Hashtbl.add dataRegs impId dataReg;
    Hashtbl.add dynTypes impId dynT; 
    if not $ DynType.is_scalar dynT then  
      ignore (self#fresh_shape_reg dataReg (DynType.nest_depth dynT))
    ;
    dataReg
 
  method declare_shared_vec (id : ID.t) dynEltT (dims : int list)  = 
    Hashtbl.add dynTypes id dynEltT;
    let ptxEltType = PtxType.of_dyn_type dynEltT in
    let dimsArray = Array.of_list dims in 
    let sharedId = self#fresh_shared_id in
    let nelts = Array.fold_left ( * ) 1 dimsArray in 
    let decl = {
      t = ptxEltType; 
      decl_space = SHARED;
      array_size = Some nelts;
      init_val=None;
    } in
    self#add_alloc sharedId decl; 
    let sharedVal = Sym { id=sharedId; ptx_type=PtxType.ptrT; space=SHARED } in 
    (* access the shared memory via the register that holds its address *) 
    let sharedReg = self#fresh_reg PtxType.ptrT in 
    self#emit [mov sharedReg sharedVal];
    Hashtbl.add dataRegs id sharedReg;
    (* associate the register storing the shared address with the dims *) 
    Hashtbl.add sharedDims (PtxVal.get_id sharedReg) dimsArray;  
    sharedReg  
 
 (* we've already declared the array, have a pointer into it and 
    have even allocated the slice destination. We just need to track the 
    dimensions of the slice. We expect the caller to actually compute the 
    value of sliceReg. 
 *)  
  method declare_slice ptrReg sliceReg =
    if self#is_shared_ptr ptrReg then  
      let dims = self#get_shared_dims ptrReg in
      let rank = Array.length dims in 
      if rank  < 2 then 
        failwith "[ptx_codegen->declared_shared_slice] insufficient rank"
      else begin   
        let sliceDims = Array.init (rank - 1) (fun idx -> dims.(idx+1)) in
        (* the value of sliceReg needs to get computed by the caller *)  
        Hashtbl.add sharedDims (PtxVal.get_id sliceReg) sliceDims
      end 
   else if self#is_global_array_ptr ptrReg then
     let rank = self#get_array_rank ptrReg in 
     if rank < 2 then 
       failwith "[ptx_codegen->declare_global_slice] insufficient rank" 
     else begin   
       let shapeReg = self#get_shape_reg ptrReg in 
       let sliceShapeReg = self#fresh_shape_reg sliceReg (rank - 1) in
       (* increment pointer to shape by 4 bytes to start at next dim *) 
       self#emit [add PtxType.ptrT sliceShapeReg shapeReg (int 4)]  
     end
      
  method declare_arg id dynT = 
    Hashtbl.add dynTypes id dynT; 
    let paramId = self#fresh_arg_id in  
    let gpuStorageT = PtxType.storage_of_dyn_type dynT in  
    self#add_param_decl paramId gpuStorageT;
    let dataParam =  Sym { id = paramId; ptx_type=gpuStorageT; space=PARAM} in 
    let storageReg = self#fresh_reg gpuStorageT in 
    self#emit [ld_param gpuStorageT storageReg dataParam];
    let gpuT = PtxType.of_dyn_type dynT in
    let dataReg =  
        (* 
           if storage and register types aren't the same, 
           then we need conversion code (for types like bool, u8, etc..)
        *) 
        if gpuT = gpuStorageT then storageReg 
        else self#convert_fresh ~destType:gpuT ~srcVal:storageReg  
    in 
    Hashtbl.add dataRegs id dataReg;
    (* vectors need an additional param for their shape *) 
    if not $ DynType.is_scalar dynT then  (
      let rank = DynType.nest_depth dynT in 
      let shapeReg = self#fresh_shape_reg dataReg rank in
      let shapeParamId = self#get_sym_id ("shape" ^ (string_of_int paramId)) in 
      let shapeParam = 
        Sym {id=shapeParamId; ptx_type=PtxType.ptrT; space=PARAM}  
      in 
      self#add_param_decl shapeParamId PtxType.ptrT ; 
      self#emit [ld_param PtxType.ptrT shapeReg shapeParam];
    );
    dataReg
   

  
  (* Need to consider the destination of a conversion when figuring out the
     type of the source since there is an implicit polymorphism 
     in PTX for scalar constants...
     which can be moved to registers of varying size
  *) 
  val type_of_conversion_source = fun destType srcVal -> 
    match srcVal with   
      | PtxVal.Sym _ -> PtxVal.type_of_var srcVal 
      | PtxVal.FloatConst _ ->
          if PtxType.is_float destType then destType 
          else PtxType.F64
      | PtxVal.IntConst _ -> 
          if PtxType.is_int destType then destType
          else PtxType.S64
      | _ -> failwith "[ptx_codegen] don't know how to convert this value"
    
  
  (* 
     first create a register then convert value to fit in that register,
     and lastly return the register. Might also just return the srcVal 
     if no conversion is necessary. 
  *) 
  method convert_fresh 
      ~(destType: PtxType.ty) 
      ~(srcVal: PtxVal.value) : PtxVal.value  =
    let srcType = type_of_conversion_source destType srcVal in 
    if destType = srcType then srcVal 
    else 
      let destReg = self#fresh_reg destType in 
      (self#convert ~destReg ~srcVal; destReg) 
    
  (* convert from srcType to destType, with special consideration 
     given to Predicates. This would be nice to put in PtxHelpers, 
     but must live here because it requires allocation of a register.
     Does not return anything.   
  *)   
  
  method convert 
      ~(destReg : PtxVal.value) 
      ~(srcVal : PtxVal.value) : unit =
    let destType = PtxVal.type_of_var destReg in 
    let srcType = type_of_conversion_source destType srcVal in  
    if srcType = destType then self#emit [mov destReg srcVal]
    else 
      if destType = Pred then 
        self#emit [setp_eq srcType destReg srcVal (int 0)]
      else if srcType = Pred then 
        self#emit [selp destReg (int 1) (int 0) srcVal]
      else if PtxType.is_int srcType && PtxType.is_float destType then
        self#emit [
          round Ptx.RoundNearest (cvt destType srcType destReg srcVal)
        ]
      else self#emit [cvt destType srcType destReg srcVal]
      
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
    
  method finalize_kernel =
    debug "[ptx_codegen] finalizing ptx kernel"; 
    PtxTidy.cleanup_kernel instructions allocations; 
    { 
      params = DynArray.to_array parameters; 
      code = DynArray.to_array instructions; 
      decls = allocations;
      symbols = symbols; 
      (* TODO: distinguish between textured and global inputs *)
      textures = Hashtbl.create 1; 
    }


end
