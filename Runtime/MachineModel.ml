type gpu = {
  id : int;
  name : string;
  global_mem : int;
  shared_mem_per_sm : int;
  regs_per_block : int;
  warp_size : int;
  mem_pitch : int;
  max_threads_per_block : int;
  max_threads_per_x : int;
  max_threads_per_y : int;
  max_threads_per_z : int;
  max_grid_size_x : int;
  max_grid_size_y : int;
  clock_rate_ghz : float;
  total_const_mem : int;
  accessible_peers : int array;
  global_mem_id : int;
  peak_global_bw : float;
}

type cpu = {
  id : int;
  name : string;
  clock_rate_ghz : float;
}

type t = {
  gpus : gpu array;
  cpus : cpu array;
  total_ram : int;
}

let build_machine_model =
  let homedir = Sys.getenv "HOME" in
  let conffile = String.concat "/" [homedir; ".parakeet/parakeetconf.xml"] in
  let x = Xml.parse_file conffile in
  Printf.printf "%s\n\n" (Xml.to_string_fmt x);
  x
