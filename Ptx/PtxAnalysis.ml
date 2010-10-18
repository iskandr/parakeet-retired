open Ptx 

(*
module type LATTICE = sig 
  type t 
  val bottom : t 
  val top : t 
  val bottom_of_type : PtxType.ty -> t 
  val top_of_type : PtxType.ty -> t 
  val join : t -> t -> t
  val meet : t -> t -> t 
end 
*)

type 'a ptx_lattice = { 
  
  bottom_of_type : Ptx.ty -> 'a; 
  top_of_type : Ptx.ty -> 'a; 
  combine : 'a -> 'a -> 'a; 
} 

type 'a ptx_analysis = {
  is_true : 'a -> bool option; 
  eval_sqrt : 'a -> 'a; 
  eval_rsqrt : 'a -> 'a;
  eval_lg2 : 'a -> 'a; 
  eval_ex2 : 'a -> 'a; 
  eval_not : 'a -> 'a; 
  eval_add : 'a -> 'a -> 'a; 
  eval_sub : 'a -> 'a -> 'a; 
  eval_mul : 'a -> 'a -> 'a; 
  eval_div : 'a -> 'a -> 'a; 
  eval_or : 'a -> 'a -> 'a; 
  eval_and : 'a -> 'a -> 'a; 
  eval_min : 'a -> 'a -> 'a; 
  eval_max : 'a -> 'a -> 'a;
  eval_abs : 'a -> 'a; 
}
 
let interval_analysis = { 
  is_true = (fun i -> 
    if i = IntervalDomain.IntInterval(1L, 1L) then Some true
    else if i = IntervalDomain.IntInterval(0L, 0L) then Some false
    else None
  )
  eval_sqrt = (function 
    | IntInterval _ -> failwith "sqrt not implemented for ints"
    | FloatInterval (l, u) -> 
      if u < 0. then FloatEmpty else
      let l' = if l >= 0. then l else 0. in 
      FloatInterval (sqrt l', sqrt u)
  ) 
    
let interval_lattice = { 
  bottom_of_type = (fun t -> IntervalDomain. 

module Make (L : LATTICE) = struct 
  type linenum = int 
  
  let compute_label_lines code = 
    let labelLines : (Ptx.label, linenum) Hashtbl.t = Hashtbl.create 127 in 
    for i = 0 to Array.length code - 1 do
       match code.(i).label with 
        | None -> ()
        | Some label -> Hashtbl.add labelLines  label i
    done;
    labelLines
  
  (* create an initial abstract map by setting all parameters to L.top *)
  (* and setting all other values to L.bottom (underspecified) *)
  let init_env decls params = 
    let paramEnv = 
      Array.fold_left 
        (fun map (symid, ty) -> PMap.add symid (L.top_of_type ty) map)
        PMap.empty 
        params 
    in 
    Hashtbl.fold 
      (fun symid decl map -> PMap.add symid (L.bottom_of_type decl.t) map)
      decls 
      paramEnv 
     
  
  (* for every line we return a mapping from symbols to abstract values *) 
  let forward_analysis (k:Ptx.kernel) : (PtxVal.symid, L.t) array  = 
    let initMap = init_map k.decls k.params in  
    let n = Array.length kernel.code in
    let envs = Array.create n initMap in  
    let lines = compute_label_lines k.code in 
    let workqueue : linenum Queue.t = Queue.create () in
    (* forward analysis starts at first line of code and steps through *)
    (* the code in the order it would be executed *) 
    Queue.add 0 workqueue; 
    while not (Queue.is_empty workqueue) do 
      let startPos = Queue.pop workqueue in
      (* trace a path through the program, keeping track if anything changes*)
      (* along the way *) 
      let changed = ref false in
      let currPos = ref startPos in
      (* this becomes false if we hit an Exit node or the end of the code *) 
      let keepGoing = ref true  in 
      while !keepGoing do  
        (* get the last variable->value mapping at this position *) 
        let env = envs.(!currPos) in
        let instr = code.(!currPos) in
        (* most instructions transfer control directly *)
        let nextPos = ref (!currPos + 1);
        let predicated = instr.pred <> Ptx.NoGuard in
        if Ptx.is_assignment instr.op then begin 
          (* id being set at this instruction *)   
          let id = PtxVal.get_id instr.args.(0) in 
          let oldVal = PMap.find id env in 
          let newVal = match instr.op with 
          | MathLogic(op, _) 
          | Cvt _  
          | Setp _  
          | Set _  
          | Selp _  
          | Setp (comp, _) 
          | Slct (t1,t2) 
          | Ld (_, t, _) -> L.top   
          | Mov _ ->  get_val env args.(1) 
          | _ -> failwith "expected assignment instruction"
          in 
          if newVal <> oldVal then ( 
            envs.(!currPos) <- PMap.add id newVal env; 
            changed := true
          ) 
           
        end   
        else
          match instr.op with  
          | Bar d 
          | St _ -> ()
          | Bra label ->  
            let line = Hashtbl.find lines label in
            (* predicated branching means we might either continue or branch *) 
            Queue.add line workqueue; 
            if predicated && !currPos < Array.length code  then 
              Queue.add (!currPos+1) workqueue 
            ; 
            keepGoing := false 
          | Exit -> if not predicated then keepGoing := false  
          | _ -> failwith "expected non-assignment instruction"
        in 
        if !nextPos > Array.length code - 1 then keepGong := false
        else currPos := !nextPos 
     done;      
     if !changed then Queue.add startPos workqueue
    done; 
    envs 
end
