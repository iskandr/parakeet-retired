open Base

type cpu_timer = {
  mutable start_time : float;
  mutable cpu_time : float;
  mutable running : bool;
}

type gpu_timer = {
  gpu_start : CudaEvent.CuEvent.t;
  gpu_end : CudaEvent.CuEvent.t;
  mutable gpu_time : float;
  mutable gpu_running : bool;
}

type timer =
  | CpuTimer of cpu_timer
  | GpuTimer of gpu_timer

let timers : (string, timer) Hashtbl.t = Hashtbl.create 13

let get_time () = Unix.gettimeofday ()

let mk_gpu_timer name =
  let t =
    GpuTimer {
        gpu_start = CudaEvent.cuda_create_event ();
        gpu_end = CudaEvent.cuda_create_event ();
        gpu_time = 0.0;
        gpu_running = false
    }
  in
  Hashtbl.replace timers name t;
  t

let mk_cpu_timer name =
  let timer =
    CpuTimer { start_time = get_time(); cpu_time = 0.0; running = false }
  in
  Hashtbl.replace timers name timer;
  timer

let clear = function
  | CpuTimer timer -> begin
      timer.running <- false;
      timer.start_time <- 0.0;
      timer.cpu_time <- 0.0
    end
  | GpuTimer timer -> begin
      timer.gpu_time <- 0.0;
      timer.gpu_running <- false
    end

let start = function
  | CpuTimer timer -> begin
      timer.start_time <- get_time();
      timer.running <- true
    end
  | GpuTimer timer -> begin
      CudaEvent.cuda_record_event timer.gpu_start;
      timer.gpu_running <- true
    end

let stop = function
  | CpuTimer timer -> (
      if timer.running then (
        let currTime = get_time() in
        timer.running <- false;
        let extra = currTime -. timer.start_time in
        timer.cpu_time <- timer.cpu_time +. extra
      ))
  | GpuTimer timer -> (
      if timer.gpu_running then (
        let elapsedTime =
          CudaEvent.cuda_stop_event_and_get_elapsed_time
            timer.gpu_start
            timer.gpu_end
        in
        timer.gpu_running <- false;
        timer.gpu_time <- timer.gpu_time +. elapsedTime
      ))

let stop_all () = Hashtbl.iter (fun _ timer -> stop timer) timers

let clear_all () = Hashtbl.iter (fun _ timer -> clear timer) timers

let get_total = function
  | CpuTimer timer -> begin
    let extra =
      if timer.running then get_time() -. timer.start_time else 0.0
    in
    timer.cpu_time +. extra
    end
  | GpuTimer timer -> begin
    let extra =
      if timer.gpu_running then
        CudaEvent.cuda_stop_event_and_get_elapsed_time
          timer.gpu_start timer.gpu_end
      else
        0.0
    in
    (timer.gpu_time +. extra) /. 1000.0
    end

let print_timers () =
  stop_all ();
  let print name timer =
    Printf.printf "%s: %f\n" name (get_total timer)
  in
  Hashtbl.iter print timers;
  Pervasives.flush_all()

let runTemplate = mk_cpu_timer "RunTemplate"
let untypedOpt = mk_cpu_timer "Untyped Optimizations"
let typedOpt = mk_cpu_timer "Typed Optimizations"
(*
let ptxCompile = mk_cpu_timer "PTX Compile"
let gpuTransfer = mk_cpu_timer "GPU Transfer"
let gpuExec = mk_gpu_timer "GPU Execution"
let gpuMalloc = mk_gpu_timer "GPU Memory Allocation"

let gpuMap = mk_gpu_timer "GPU Array Op - Map"
let gpuMapAlloc = mk_gpu_timer "GPU Array Op - Map (Alloc)"

let gpuReduce = mk_gpu_timer "GPU Array Op - Reduce"
let gpuReduceAlloc = mk_gpu_timer "GPU Array Op - Reduce (Alloc)"

let gpuIndex = mk_gpu_timer "GPU Array Op - Index"
let gpuIndexAlloc = mk_gpu_timer "GPU Array Op - Index (Alloc)"

let gpuWhere = mk_gpu_timer "GPU Array Op - Where"
let gpuWhereAlloc = mk_gpu_timer "GPU Array Op - Where (Alloc)"

let gpuFlip = mk_gpu_timer "GPU Array Op - Flip"
let gpuFlipAlloc = mk_gpu_timer "GPU Array Op - Flip (Alloc)"
*)