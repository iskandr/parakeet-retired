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

type cache = {
  id : int;
  level : int;
  cores : int list;
  size_kb : int;
  line_size_bytes : int;
  associativity : int;
}
and
core = {
  id : int;
  caches : int list;
  thread_affinity_ids : int list;
}

type cpu = {
  id : int;
  name : string;
  clock_rate_ghz : float;
  cores : core array;
  caches : cache array;
}

type t = {
  gpus : gpu array;
  cpus : cpu array;
  total_ram : int;
}

let build_machine_model =
  let homedir = Sys.getenv "HOME" in
  let gpufile = homedir ^ "/.parakeet/parakeetgpuconf.xml" in
  let gpuxml = Xml.parse_file gpufile in
  (*Printf.printf "%s\n\n" (Xml.to_string_fmt x);
  *)
  let cpufile = homedir ^ "/.parakeet/parakeetcpuconf.xml" in
  let cpuxml = Xml.parse_file cpufile in
  gpuxml
