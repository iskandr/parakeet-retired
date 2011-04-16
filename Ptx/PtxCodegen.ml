(* pp: -parser o pa_macro.cmo *)

(* state of the code generator includes:
    -- instructions thus far emitted
    -- free registers of all GPU types
    -- total number of registers of all GPU types
        (used for generating allocations)

    This module also has code generation helper functions
*)

open Base
open Ptx
open PtxType 
open PtxVal

let initialNumRegs = 101 
  
class ptx_codegen = object (self)  
 
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
    
  method value_to_str ptxVal = PtxVal.to_str symbols ptxVal 
        
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

  (* GENERATE FRESH NAMES FOR TEXTURE REFERENCES *)
  val mutable numTex = 0
  method private fresh_tex_id =
    let num = numTex in
    numTex <- num + 1;
    self#get_sym_id ("tex" ^(string_of_int num))

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
  val local_allocations : (PtxVal.symid, Ptx.var_decl) Hashtbl.t =
     Hashtbl.create initialNumRegs
  val global_allocations : (PtxVal.symid, Ptx.var_decl) Hashtbl.t = 
     Hashtbl.create 13 
  
  method private add_local_alloc id newAlloc = 
    Hashtbl.add local_allocations id newAlloc 
  
  method private add_global_alloc id newAlloc = 
    Hashtbl.add global_allocations id newAlloc 

  (* FUNCTION PARAMETERS *) 
  val parameters: (PtxVal.symid * PtxType.ty) DynArray.t = DynArray.create ()
  method private add_param_decl id newParam =
    DynArray.add parameters (id, newParam)
  
  (* TEXTURE REFERENCES *)
  val textures: (PtxVal.symid * PtxType.ty) DynArray.t = DynArray.create ()
  method private add_tex_decl id newTex = DynArray.add textures (id, newTex)

  (* number of registers currently allocated for every type *)
  val maxRegs : (PtxType.ty, int) Hashtbl.t =  Hashtbl.create initialNumRegs
  
  (* Mapping from Imp identifiers to their corresponding registers *)
  (* Note, this also contains mappings to texture references as well *)
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
  
  method private set_global_array_rank (arrayPtr:PtxVal.value) rank = 
    Hashtbl.add globalArrayRanks (PtxVal.get_id arrayPtr) rank 
     
  method is_global_array_ptr = function 
    | PtxVal.Sym {id=id} -> Hashtbl.mem globalArrayRanks id
    | _ -> false
  
  val texRefs : (PtxVal.symid, PtxVal.value) Hashtbl.t =
    Hashtbl.create initialNumRegs
  
  method get_tex_ref (texPtr : PtxVal.value) =
    match Hashtbl.find_option texRefs (PtxVal.get_id texPtr) with
      | Some texRef -> texRef
      | None -> failwith $ 
          "[PtxCodegen] Unable to find rank: PTX value " ^ 
          (PtxVal.to_str symbols texPtr) ^ " not registered as a texture ptr"
  
  method is_tex = function
    | PtxVal.Sym {id=id} -> Hashtbl.mem texRefs id
    | _ -> false
  
  val texGeoms : (PtxVal.value, Ptx.geom) Hashtbl.t =
    Hashtbl.create initialNumRegs

  method get_tex_geom texRef =
    if not (Hashtbl.mem texGeoms texRef) then
      failwith "[ptx_codegen] getting geom from unknown texture";
    Hashtbl.find texGeoms texRef

  (* works for both shared and global vectors *) 
  method get_array_rank (ptr: PtxVal.value) =
    if self#is_shared_ptr ptr then Array.length (self#get_shared_dims ptr)
    else if self#is_global_array_ptr ptr then self#get_global_array_rank ptr
    else
      failwith $
        "[ptx_codegen] can't get array rank of non-array register: " ^
        (PtxVal.to_str symbols ptr)

  (* get a register which points to the shape vector attached to the 
     argument "ptrReg" which contains the address of some array's data. 
  *) 
  method private fresh_shape_reg ptrReg rank =
    let shapeReg = self#fresh_reg PtxType.ptrT in
    let ptrId = PtxVal.get_id ptrReg in
    Hashtbl.add shapeRegs ptrId shapeReg;
    self#set_global_array_rank ptrReg rank;
    shapeReg
    
  val mutable maxShapeOffset = 0
  
  method private incr_shape_offset rank = 
    let currOffset = maxShapeOffset in 
    maxShapeOffset <- maxShapeOffset + rank * 4; 
    currOffset  
  
  (* shapes are stored in constant memory at a known offset *) 
  val shapeOffsets : (PtxVal.symid, int) Hashtbl.t = 
    Hashtbl.create initialNumRegs 
  
  (* returns a register holding a pointer to the shape in constant memory *) 
  method private register_input_shape ptrReg rank =
    let shapeReg = self#fresh_shape_reg ptrReg rank in 
    let offset = self#incr_shape_offset rank in   
    Hashtbl.replace shapeOffsets (PtxVal.get_id ptrReg) offset;
    let constants = self#get_constants in  
    self#emit [Ptx.add PtxType.ptrT shapeReg constants (Ptx.int offset)]; 
    shapeReg, offset 
  
  method fresh_reg gpuT =
    let reg_name gpuT id = Printf.sprintf "r%s%d" (PtxType.suffix gpuT) id in
    let currMax = match Hashtbl.find_option maxRegs gpuT with
      | Some max -> max
      | None -> 0
    in
    Hashtbl.replace maxRegs gpuT (currMax + 1);
    let id = self#get_sym_id (reg_name gpuT currMax) in
    self#add_local_alloc id {
      t = gpuT;
      decl_space = REG;
      array_size = None;
      init_val= None
    };
    Sym {id=id; ptx_type=gpuT; space=REG}
  
 (** returns a list of register ids for requested type **)
  method fresh_regs gpuT count =
    Array.init count (fun _ -> self#fresh_reg gpuT)



  method declare_shared_vec (id : ID.t) dynEltT (dims : int list) =
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
    self#add_local_alloc sharedId decl;
    let sharedVal = Sym { id=sharedId; ptx_type=PtxType.ptrT; space=SHARED } in
    (* access the shared memory via the register that holds its address *)
    let sharedReg = self#fresh_reg PtxType.ptrT in
    self#emit [mov sharedReg sharedVal];
    Hashtbl.add dataRegs id sharedReg;
    (* associate the register storing the shared address with the dims *)
    Hashtbl.add sharedDims (PtxVal.get_id sharedReg) dimsArray;
    sharedReg
 
  val dataLayouts : (PtxVal.value, GpuVal.data_layout) Hashtbl.t = 
    Hashtbl.create 127  

  method private get_data_layout reg = 
    Hashtbl.find_default dataLayouts reg GpuVal.RowMajor
  
  method private set_data_layout reg layout = 
    Hashtbl.replace dataLayouts reg layout 
  
  method private is_column_major reg = 
    self#get_data_layout reg = GpuVal.ColumnMajor

  (* ancestor array of some slice register *)  
  val sliceRoots : (PtxVal.symid, PtxVal.value) Hashtbl.t = Hashtbl.create 127
  
  method private get_slice_root reg : PtxVal.value option =
    let symid = PtxVal.get_id reg in 
    if Hashtbl.mem sliceRoots symid then 
        Some (Hashtbl.find sliceRoots symid)
    else None  
   
  (* we've already declared the array, have a pointer into it and 
     have even allocated the slice destination. We just need to track the 
     dimensions of the slice. We expect the caller to actually compute the 
     value of sliceReg. 
  *)  
  method declare_slice (ptrReg : PtxVal.value) (sliceReg:PtxVal.value) =
    (match self#get_slice_root ptrReg with 
      | None -> 
        (* if we're slicing into the original array, register it as our root *)
        Hashtbl.add sliceRoots (PtxVal.get_id sliceReg) ptrReg
      | Some prevRoot -> 
        Hashtbl.add sliceRoots (PtxVal.get_id sliceReg) prevRoot
    );   
   
    if self#is_shared_ptr ptrReg then (
     
      let dims = self#get_shared_dims ptrReg in
      let rank = Array.length dims in
      if rank < 2 then
        failwith "[ptx_codegen->declared_shared_slice] insufficient rank"
      else (
        let sliceDims = Array.init (rank - 1) (fun idx -> dims.(idx+1)) in
        (* the value of sliceReg needs to get computed by the caller *)
        Hashtbl.add sharedDims (PtxVal.get_id sliceReg) sliceDims
      )
    )
    else
      let rank = self#get_array_rank ptrReg in
      
      if rank < 2 then
        failwith "[ptx_codegen->declare_global_slice] insufficient rank" 
      else (
        IFDEF DEBUG THEN 
          Printf.printf "In declare_slice, rank=%d, is_tex=%B!\n" 
           rank
           (self#is_tex ptrReg)
        ENDIF; 
        if self#is_tex ptrReg then (
            let texRef = Hashtbl.find texRefs (PtxVal.get_id ptrReg) in            
            Hashtbl.add texRefs (PtxVal.get_id sliceReg) texRef
        ); 
        if self#is_column_major ptrReg then 
           self#set_data_layout sliceReg GpuVal.ColumnMajor
        ;
        (* SUPER HACKISH-- even if arrays are in column-major layout, 
           maintain a row-major shape associated with the pointer
           so that DimSize can be compiled correctly 
         *)
        
        let shapeReg = self#get_shape_reg ptrReg in 
        let sliceShapeReg = self#fresh_shape_reg sliceReg (rank - 1) in
        (* increment pointer to shape by 4 bytes to start at next dim *)
        self#emit [add PtxType.ptrT sliceShapeReg shapeReg (int 4)]
      )

  (* CONSTANTS TABLE *) 
  val mutable constants = None
  method private get_constants = match constants with 
    | None ->  
      let constId = self#get_sym_id "constants" in 
      let constTableAlloc = { 
        t = PtxType.B8; 
        decl_space = PtxVal.CONST; 
        array_size = Some 10000; 
        init_val = None; 
      } 
      in
      self#add_global_alloc constId constTableAlloc;
      let constSym = 
        {id = constId; ptx_type = PtxType.B8; space=PtxVal.CONST} 
      in
      let constVal = PtxVal.Sym constSym in
      let constPtr = self#fresh_reg PtxType.ptrT in
      self#emit [Ptx.mov ~ty:PtxType.ptrT constPtr constVal];  
      constants <- Some constPtr; 
      constPtr
    | Some ptr -> ptr  

  (***************************************************
               CALLING CONVENTIONS 
   ***************************************************)
  val paramOrder : ID.t DynArray.t = DynArray.create()
  val dataLocations 
    : (ID.t, PtxCallingConventions.data_location) Hashtbl.t = Hashtbl.create 127 

  (* shared between storage arguments-- used for local vectors, 
     and global input arguments 
  *) 
  method private declare_param 
      (id : ID.t) 
      (dynT : DynType.t)
      (dataLayout:GpuVal.data_layout) : PtxVal.value =
    Hashtbl.add dynTypes id dynT;
    DynArray.add paramOrder id;
    let paramId = self#fresh_arg_id in
    let gpuStorageT = PtxType.storage_of_dyn_type dynT in
    self#add_param_decl paramId gpuStorageT;
    let dataParam = Sym {id=paramId; ptx_type=gpuStorageT; space=PARAM} in
    let storageReg = self#fresh_reg gpuStorageT in
    self#emit [ld_param gpuStorageT storageReg dataParam];
    let gpuT = PtxType.of_dyn_type dynT in
    let dataReg =
        (* if storage and register types aren't the same, then convert *)
        if gpuT = gpuStorageT then storageReg
        else self#convert_fresh ~destType:gpuT ~srcVal:storageReg
    in
    Hashtbl.add dataRegs id dataReg;
    Hashtbl.add dataLayouts dataReg dataLayout;
    dataReg
    
  method private declare_input_param 
                  (id:ID.t) 
                  (dynT:DynType.t) 
                  (dataLayout : GpuVal.data_layout) :  PtxVal.value =
    let dataReg = self#declare_param id dynT dataLayout in  
    (* vectors need an additional param for their shape *)
    if DynType.is_scalar dynT then
      Hashtbl.add dataLocations id PtxCallingConventions.ScalarInput 
    else (
      let rank = DynType.nest_depth dynT in
      let shapeReg, shapeOffset = self#register_input_shape dataReg rank in
      let loc = PtxCallingConventions.GlobalInput (dataLayout, shapeOffset) in  
      Hashtbl.add dataLocations id loc
    );
    dataReg
  
  
    

  (*
     if variable is a scalar then allocates one new register and returns
     it. if it's a vector then allocate a register pair (data/shape) and
     return the data register. If the vector is a vector and requires 
     storage (meaning it isn't just a slice through an existing vector)
     then the optional argument external_alloc is set to true and a parameter 
     is registered for the external storage.
  *) 
  method declare_local impId dynT =
    let ptxT = PtxType.of_dyn_type dynT in
    let dataReg = self#fresh_reg ptxT in
    IFDEF DEBUG THEN 
      Printf.printf "[PtxCodegen] Registering Imp ID %s as %s\n"
        (ID.to_str impId)
        (PtxVal.to_str symbols dataReg)
    ENDIF; 
    Hashtbl.add dataRegs impId dataReg;
    Hashtbl.add dynTypes impId dynT;
    
    if not $ DynType.is_scalar dynT then
      ignore (self#fresh_shape_reg dataReg (DynType.nest_depth dynT))
    ;
    dataReg
   
  
  (* Texture inputs *)
  method private declare_tex_param impId dynT dataLayout =
    Hashtbl.add dynTypes impId dynT;
    DynArray.add paramOrder impId;
    let texId = self#fresh_tex_id in
    let ptxT = PtxType.of_dyn_type dynT in
    let texParam = Sym {id=texId; ptx_type=ptxT; space=TEX} in
    self#add_tex_decl texId ptxT;
    let basePtr = self#fresh_reg PtxType.S32 in
    let baseId = PtxVal.get_id basePtr in
    self#emit [mov basePtr (int 0)];
    Hashtbl.add dataRegs impId basePtr;
    Hashtbl.add dataLayouts basePtr dataLayout; 
    Hashtbl.add texRefs baseId texParam;
    let rank = DynType.nest_depth dynT in
    (* We also do something really dumb here, which is just to pick a shape *)
    (* that matches the input's rank (1D for 1D, etc.) -- should surely *)
    (* revisit later *)
    let name = Hashtbl.find symbols texId in
    (*let geom = num_to_geom rank in*) 
    (* TODO: actually do the right geom thing *)
    let geom = Ptx.Tex1D in
    Hashtbl.add texGeoms texParam geom;
    (* still need to pass down shapes of inputs bound to textures *)
    let shapeReg, shapeOffset = self#register_input_shape basePtr rank in
    let cc = 
      PtxCallingConventions.TextureInput(name, geom, dataLayout, shapeOffset) 
    in
    Hashtbl.add dataLocations impId cc;
    shapeReg
  
  (*************************************************************
                      CACHE ACCESS TO SPECIAL REGISTERS
   *************************************************************)
  
  (* don't allocate registers for special registers until 
     you need them 
  *) 
  val registerCache 
    : (PtxVal.value * PtxType.ty, PtxVal.value) Hashtbl.t = Hashtbl.create 17
  method cached ?ty ptxVal = 
    let ty = match ty with 
        | Some ty -> ty 
        | None -> PtxVal.type_of_value ptxVal 
    in 
    let key = ptxVal, ty in 
    if Hashtbl.mem registerCache key then 
      Hashtbl.find registerCache key
    else (
      let reg = self#fresh_reg ty in 
      self#emit [mov reg ptxVal];
      Hashtbl.add registerCache key reg; 
      reg
    )
  method private is_cached ptxVal ty = Hashtbl.mem registerCache (ptxVal,ty)
    
  method private tid_x = self#cached tid.x
  method private tid_y = self#cached tid.y
  method private tid_z = self#cached tid.z
 
  method private ntid_x = self#cached ntid.x 
  method private ntid_y = self#cached ntid.y
  method private ntid_z = self#cached ntid.z 
  
  method private ctaid_x = self#cached ctaid.x 
  method private ctaid_y = self#cached ctaid.y
  
  method private nctaid_x = self#cached nctaid.y    
  method private nctaid_y = self#cached nctaid.y
  
  (*************************************************************
                      INDEX COMPUTATIONS
   *************************************************************) 
  
  val mutable threadsPerBlock : PtxVal.value option = None
  method compute_threads_per_block = match threadsPerBlock with 
    | Some reg -> reg
    | None -> 
        let t1, t2 = self#fresh_reg U32, self#fresh_reg U32 in
        self#emit [
          comment "computing threads per block";
          mul_wide U16 t1 (self#cached ntid.x)  (self#cached ntid.y);
          mul U32 t2 t1 (self#cached ~ty:U32 ntid.z)
        ];
        threadsPerBlock <- Some t2; 
        t2 
  
  val mutable linearBlockId : PtxVal.value option = None
  method compute_linear_block_index = match linearBlockId with 
    | Some reg -> reg
    | None -> 
        let x = self#cached nctaid.x in 
        let y = self#cached nctaid.y in
        let blockId = self#fresh_reg U32 in 
        (* grids can only be two-dimensional, so z component always = 1 *) 
        self#emit [mul_wide U16 blockId x y];
        linearBlockId <- Some blockId;
        blockId
  
  (* which thread are you in your block? *)
  val mutable linearBlockOffset : PtxVal.value option = None
  method compute_linear_block_offset = match linearBlockOffset with 
    | Some reg -> reg
    | None -> 
        let regs = self#fresh_regs U32 5 in
        self#emit [
          comment "compute linear index within block";
          mul_wide U16 regs.(0) (self#cached ntid.y) (self#cached ntid.z);
          mul U32 regs.(1) (self#cached ~ty:U32 tid.x) regs.(0); 
          mul_wide U16 regs.(2) (self#tid_y) (self#ntid_z); 
          add U32 regs.(3) regs.(1) regs.(2);
          add U32 regs.(4) (self#cached ~ty:U32 tid.z) regs.(3) 
        ];
        linearBlockOffset <- Some regs.(4); 
        regs.(4)
           
  (* which thread are you in the entire computational grid? *) 
  val mutable linearThreadIndex : PtxVal.value option =  None
  method compute_linear_thread_index = match linearThreadIndex with 
    | Some reg -> reg
    | None ->
      let blockId = self#compute_linear_block_index in 
      let threadsPerBlock = self#compute_threads_per_block in
      let blockOffset = self#compute_linear_block_offset in 
      let regs = self#fresh_regs U32 2 in 
      self#emit [
        comment "compute linear thread index";
        mul U32 regs.(0) blockId threadsPerBlock; 
        add U32 regs.(1) blockOffset regs.(0)
      ]; 
      linearThreadIndex <- Some regs.(1); 
      regs.(1)      
      
  val calc_index_widths = fun dims eltSize ->
    let n = Array.length dims in 
    let result = Array.copy dims in 
    result.(n-1) <- eltSize; 
    for i = n - 2 downto 0 do 
      result.(i) <- dims.(i+1) * result.(i+1)
    done;
    result

  (* compute the absolute address of an element in an array
     given that array's base address register, the size of the
     elements contained in the array and an array of registers
     containing indices. Assumes that the baseReg has been
     registered either as a shared array, an array input,
     or a "local" storage array. 
  *)
  method compute_address baseReg eltSize idxRegs =
    let rank = self#get_array_rank baseReg in
    let numIndices = Array.length idxRegs in
    let baseReg64 = self#convert_fresh ~destType:U64 ~srcVal:baseReg in
    let idxRegs64 =
      Array.map
        (fun reg -> self#convert_fresh ~destType:U64 ~srcVal:reg)
        idxRegs
    in
    let address = self#fresh_reg U64 in
    self#emit [mov address baseReg64]; 
    begin
    if self#is_shared_ptr baseReg then
      let widths = calc_index_widths (self#get_shared_dims baseReg) eltSize in
      for i = 0 to numIndices - 1 do
        let multReg = self#fresh_reg U64 in
        let width = int widths.(i) in
        self#emit [
          mul_lo U64 multReg idxRegs64.(i) width;
          add U64 address address multReg
        ]
      done
    (* COLUMN MAJOR INDEXING, BITSNITCHES *) 
    else if self#is_column_major baseReg then (
        assert (Array.length idxRegs = 1); 
        let root : PtxVal.value = match self#get_slice_root baseReg with 
          | Some root -> root 
          | None -> baseReg 
        in
        let rootRank = self#get_array_rank root in
        let rootShape = self#get_shape_reg root in  
        let currRank = self#get_array_rank baseReg in 
        let multiplier = self#fresh_reg U64 in
        if self#is_tex baseReg then
          self#emit [mov multiplier (int 1)]
        else
          self#emit [mov multiplier (int eltSize)]
        ;
        for i = 0 to rootRank - currRank - 1 do
          let shapeElt = self#fresh_reg U32 in
          self#emit [ld_const U32 shapeElt rootShape ~offset:(4*i)];
          let shapeElt64 = self#fresh_reg U64 in
          self#convert ~destReg:shapeElt64 ~srcVal:shapeElt;
          self#emit [mul_lo U64 multiplier multiplier shapeElt64] 
        done; 
        let offset = self#fresh_reg U64 in
        self#emit [
          mul U64 offset idxRegs64.(0) multiplier;
          add U64 address address offset  
        ]
    )
    else   
      (* for row major arrays, get an offset into the parent shape, 
         for column major get the full shape
      *)
      let shapeReg = self#get_shape_reg baseReg in
      for i = 0 to numIndices - 1 do
      (* size of slice through array each index accounts for *)
        let multiplier = self#fresh_reg U64 in
        (* at the very least each increase in this index has to go up by the
           bytesize of the element
        *)
        if self#is_tex baseReg then
          self#emit [mov multiplier (int 1)]
        else
          self#emit [mov multiplier (int eltSize)];
        (* indexing the first dimension effectively slices through all
           other dimensions. If this is, however, a 1D vector then this
           loop never runs
        *)
        for j = (i+1) to rank - 1 do
          let shapeEltReg = self#fresh_reg U32 in
          let shapeEltReg64 = self#fresh_reg U64 in
          self#emit [ld_const U32 shapeEltReg shapeReg ~offset:(4*j)];
          self#convert ~destReg:shapeEltReg64 ~srcVal:shapeEltReg;
          self#emit [mul_lo U64 multiplier multiplier shapeEltReg64]
        done;
        let offset = self#fresh_reg U64 in
        self#emit [
          mul U64 offset idxRegs64.(i) multiplier;
          add U64 address address offset  
        ]
      done
    end;
    address

  (* storage args are those that supply private heap space to each thread *)

  val storageArgs : ID.t DynArray.t = DynArray.create () 
  method  declare_storage_arg impId dynT =  
    (* BROKEN!!!! *) 
    Hashtbl.add dataLocations 
      impId 
      (PtxCallingConventions.GlobalInput(GpuVal.RowMajor,0));
    DynArray.add storageArgs impId; 
    (* storage is a giant vector in memory, where each 
       element of this vector corresponds to a single thread's
       storage requirements
    *)
    let giantVectorReg = 
      self#declare_input_param impId (DynType.VecT dynT) GpuVal.RowMajor 
      
    in
    (* don't map impId to the huge global vector, we expect that
       declare_local will instead later associate impId with
       this thread's local slice into the storage vector
    *)
    Hashtbl.remove dataRegs impId;
    let linearIndexReg = self#compute_linear_thread_index in
    let eltBytes =
      PtxType.nbytes
        (PtxType.storage_of_dyn_type (DynType.elt_type dynT))
    in
    (* the starting address of a thread's private slice into the 
       global storage vector
    *) 
    let address = 
      self#compute_address giantVectorReg eltBytes [|linearIndexReg|]
    in 
    self#declare_slice giantVectorReg address;
    let myStorageSlice = self#declare_local impId dynT in
    self#emit [mov myStorageSlice address];
    myStorageSlice
  
  method declare_input impId dynT ?(dataLayout=GpuVal.RowMajor) = function 
    | PtxVal.PARAM 
    | PtxVal.GLOBAL -> self#declare_input_param impId dynT dataLayout
    (* For now, assume dumbly that we can surely use textures for all 1D *)
    (* inputs that were requested to use them, and not use textures for*)
    (* anything else *)
    | PtxVal.TEX ->
        (* TODO: Let 2D inputs be bound to 1D textures *)
        (*       In order to support this, have to make indexing into them *)
        (*       return their element type, not a pointer to a slice *)
        let rank = DynType.nest_depth dynT in
        let elType = DynType.elt_type dynT in
        if rank = 1 || rank = 2 then (
          match elType with
            | DynType.UInt32T
            | DynType.Int32T
            | DynType.Float32T -> self#declare_tex_param impId dynT dataLayout
            | _ -> self#declare_input_param impId dynT dataLayout
        ) else
          self#declare_input_param impId dynT dataLayout
    | PtxVal.CONST -> assert false 
    | other -> failwith $ Printf.sprintf
        "kernel inputs via %s space not yet implemented"
        (PtxVal.ptx_space_to_str other)
  
  method declare_output impId dynT = 
    let dataReg = self#declare_param impId dynT GpuVal.RowMajor in 
    let rank = DynType.nest_depth dynT in
    let shapeReg, shapeOffset = self#register_input_shape dataReg rank in
    let loc = PtxCallingConventions.GlobalOutput shapeOffset in  
    Hashtbl.add dataLocations impId loc; 
    dataReg 
  
 
  (* Need to consider the destination of a conversion when figuring out the
     type of the source since there is an implicit polymorphism 
     in PTX for scalar constants...
     which can be moved to registers of varying size
  *) 
  val type_of_conversion_source  = 
    fun (destType : PtxType.ty) (srcVal : PtxVal.value) -> 
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
    else if PtxVal.is_ptx_constant srcVal && self#is_cached srcVal destType 
    then  
      self#cached ~ty:destType srcVal
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
        if PtxType.nbytes srcType > 1 then 
          self#emit [setp_eq srcType destReg srcVal (int 0)]
        else begin 
          (* need to store into 16-bit register first since
             setp doesn't work for 8-bit types 
          *)
          let tempType = larger_type srcType in 
          let tempReg = self#fresh_reg tempType in
          self#convert tempReg srcVal;  
          self#emit [setp_eq tempType destReg srcVal (int 0)] 
        end  
      else if srcType = Pred then 
        if PtxType.nbytes destType > 1 then
          self#emit [selp destReg (int 1) (int 0) srcVal]
        else begin  
        (*
          need to store in 16-bit register, 
          since selp doesn't work for 8-bit types 
        *)
          let tempType = larger_type destType in  
          let tempReg = self#fresh_reg tempType in  
          self#emit [selp tempReg (int 1) (int 0) srcVal];
          self#convert ~destReg ~srcVal:tempReg 
        end
      else if PtxType.is_int srcType && PtxType.is_float destType then
        self#emit [
          round Ptx.RoundNearest (cvt destType srcType destReg srcVal)
        ]
      else if PtxType.is_float srcType && PtxType.is_int destType then
        self#emit [
          round Ptx.RoundNearest_Int (cvt destType srcType destReg srcVal)
        ]
      else self#emit [cvt destType srcType destReg srcVal]
      
  method private run_rewrite_pass f =
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
    
  method finalize_kernel 
         : Ptx.kernel * PtxCallingConventions.calling_conventions =
    PtxTidy.cleanup_kernel instructions local_allocations;
    let kernel = { 
      params = DynArray.to_array parameters; 
      code = DynArray.to_array instructions; 
      local_decls = local_allocations;
      global_decls = global_allocations; 
      symbols = symbols; 
      textures = DynArray.to_array textures;
    } 
    in
    IFDEF DEBUG THEN 
      Printf.printf "Finalized PTX kernel:\n %s\n" 
        (Ptx.kernel_to_str kernel "???");  
    ENDIF; 
    let cc = { 
      PtxCallingConventions.data_locations =    
        Hashtbl.fold 
          (fun id loc env -> ID.Map.add id loc env) 
          dataLocations
          ID.Map.empty;
      param_order = DynArray.to_array paramOrder
    }
    in
    kernel, cc

end
