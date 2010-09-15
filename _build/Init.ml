open HardwareInfo
open LibPQ

(**
 * Initializes the compiler.  Called once from Q/C.
 *)
let compiler_init () =
  cuda_init ();
  hw_init ()
