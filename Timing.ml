let kernel_compile_time = ref 0.0
let mem_xfer_time = ref 0.0
let gpu_run_time = ref 0.0
let total_run_time = ref 0.0

let reset_timers () =
  kernel_compile_time := 0.0;
  mem_xfer_time := 0.0;
  gpu_run_time := 0.0;
  total_run_time := 0.0

let get_time () = Unix.gettimeofday ()

let inc_kernel_compile_time offset =
  kernel_compile_time := !kernel_compile_time +.
                         (Unix.gettimeofday ()) -. offset
let inc_mem_xfer_time offset =
  mem_xfer_time := !mem_xfer_time +.
                   (Unix.gettimeofday ()) -. offset
let inc_gpu_run_time offset =
  gpu_run_time := !gpu_run_time +.
                  (Unix.gettimeofday ()) -. offset
let inc_total_run_time offset =
  total_run_time := !total_run_time +.
                    (Unix.gettimeofday ()) -. offset

let print_timers () =
  let parakeet_time =
    !total_run_time -. !gpu_run_time -. !mem_xfer_time -. !kernel_compile_time
  in
  Printf.printf "Kernel compile time: %f\n"    !kernel_compile_time;
  Printf.printf "Memory transfer time: %f\n"   !mem_xfer_time;
  Printf.printf "GPU run time: %f\n"           !gpu_run_time;
  Printf.printf "Total run time: %f\n"         !total_run_time;
  Printf.printf "Parakeet compiler overhead\n";
  Printf.printf "  (Excluding mem xfers, PTX JIT, GPU runttime): %f\n"
    parakeet_time;
  reset_timers();
  flush stdout