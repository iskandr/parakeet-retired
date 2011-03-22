open Base 

type timer = { 
  mutable start_time : float; 
  mutable acc_time : float;
  mutable running : bool; 
  
} 

let timers : (string, timer) Hashtbl.t = Hashtbl.create 13 

let get_time () = Unix.gettimeofday ()

let mk_timer name = 
  let timer = { start_time = get_time(); acc_time = 0.0; running=false } in 
  Hashtbl.replace timers name timer; 
  timer 

let clear timer =
  timer.running <- false;
  timer.start_time <- 0.0;
  timer.acc_time <- 0.0

let start timer =
  clear timer;
  timer.start_time <- get_time();
  timer.running <- true
  
let stop timer =
  if timer.running then (
    let currTime = get_time() in
    timer.running <- false;  
    let extra = currTime -. timer.start_time in
    timer.acc_time <- timer.acc_time +. extra
  )

let stop_all () = Hashtbl.iter (fun _ timer -> stop timer) timers

let clear_all () = Hashtbl.iter (fun _ timer -> clear timer) timers 
  
let get_total timer =
  let extra = 
    if timer.running then get_time() -. timer.start_time else 0.0 
  in 
  timer.acc_time +. extra 
  
let print_timers () =
  stop_all (); 
  let print name timer = 
    Printf.printf "%s: %f\n" name (get_total timer) 
  in 
  Hashtbl.iter print timers; 
  Pervasives.flush_all()  
  
let runTemplate = mk_timer "RunTemplate"
let untypedOpt = mk_timer "Untyped Optimizations"
let typedOpt = mk_timer "Typed Optimizations"
let ptxCompile = mk_timer "PTX Compile"
let gpuTransfer = mk_timer "GPU Transfer"
let gpuExec = mk_timer "GPU Execution"
let gpuMalloc = mk_timer "GPU Memory Allocation"
