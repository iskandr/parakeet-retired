(* pp: -parser o pa_macro.cmo *)

type gpu_t = {
  gpu_id : int;
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
  gpu_clock_rate_ghz : float;
  total_const_mem : int;
  accessible_peers : int array;
  global_mem_id : int;
  peak_global_bw : float;
}

type cache_t = {
  cache_id : int;
  level : int;
  core_ids : int array;
  size_kb : int;
  line_size_bytes : int;
  associativity : int;
}

type core_t = {
  core_id : int;
  cache_ids : int array;
  thread_affinity_ids : int array;
}

type cpu_t = {
  cpu_id : int;
  cpu_clock_rate_ghz : float;
  cores : core_t array;
  caches : cache_t array;
}

type t = {
  gpus : gpu_t array;
  cpus : cpu_t array;
  total_ram : int;
}

let print_cpu cpu =
  Printf.printf "CPU id: %d\n" cpu.cpu_id;
  Printf.printf "CPU clock rate: %f\n" cpu.cpu_clock_rate_ghz;
  Printf.printf "CPU number of cores: %d\n" (Array.length cpu.cores);
  let print_core core =
    Printf.printf " Core id: %d\n" core.core_id;
    let cache_ids = List.map string_of_int (Array.to_list core.cache_ids) in
    Printf.printf " Core cache ids: %s\n" (String.concat " " cache_ids);
    let affinities =
      List.map string_of_int (Array.to_list core.thread_affinity_ids)
    in
    Printf.printf " Core affinity ids: %s\n" (String.concat " " affinities)
  in
  Array.iter print_core cpu.cores;
  Printf.printf "CPU number of caches: %d\n%!" (Array.length cpu.caches);
  let print_cache cache =
    Printf.printf " Cache id: %d\n" cache.cache_id;
    Printf.printf " Cache level: %d\n" cache.level;
    let core_ids = List.map string_of_int (Array.to_list cache.core_ids) in
    Printf.printf " Cache core ids: %s\n" (String.concat " " core_ids);
    Printf.printf " Cache size (KB): %d\n" cache.size_kb;
    Printf.printf " Cache line size: %d\n" cache.line_size_bytes;
    Printf.printf " Cache associativity: %d\n" cache.associativity
  in
  Array.iter print_cache cpu.caches

let test_tag (node:Xml.xml) (name:string) =
  String.compare (Xml.tag node) name == 0

let force_tag (node:Xml.xml) (name:string) =
  if (String.compare (Xml.tag node) name) != 0 then
    failwith "Malformed XML configuration file."

let get_tag_val (node:Xml.xml) =
  let children = Xml.children node in
  assert(List.length children == 1);
  Xml.pcdata (List.hd children)

let consume_cache_child (cache:cache_t) (node:Xml.xml) =
  if test_tag node "Id" then
    let id = int_of_string (get_tag_val node) in
    {cache with cache_id = id}
  else if test_tag node "Level" then
    let level = int_of_string (get_tag_val node) in
    {cache with level = level}
  else if test_tag node "Cores" then
    let core_nodes = Xml.children node in
    let num_cores = (List.length core_nodes) in
    let cores = Array.make num_cores (-1) in
    let rec get_core_ids i = function
      | hd :: rest ->
        Array.set cores i (int_of_string (get_tag_val hd));
        get_core_ids (i+1) rest
      | [] -> ()
    in
    get_core_ids 0 core_nodes;
    {cache with core_ids = cores}
  else if test_tag node "Size" then
    let size = int_of_string (get_tag_val node) in
    {cache with size_kb = size}
  else if test_tag node "LineSize" then
    let line_size = int_of_string (get_tag_val node) in
    {cache with line_size_bytes = line_size}
  else if test_tag node "Associativity" then
    let associativity = int_of_string (get_tag_val node) in
    {cache with associativity = associativity}
  else failwith ("Unexpected Cache XML child: " ^ (Xml.tag node))

let build_cache (node:Xml.xml) (cpu:cpu_t) =
  let children = Xml.children node in
  let dummy_cache = {
    cache_id = -1;
	  level = -1;
	  core_ids = Array.make 0 0;
	  size_kb = -1;
	  line_size_bytes = -1;
	  associativity = -1
	} in
  List.fold_left consume_cache_child dummy_cache children

let consume_core_child (core:core_t) (node:Xml.xml) =
  if test_tag node "Id" then
    let id = int_of_string (get_tag_val node) in
    {core with core_id = id}
  else if test_tag node "Threads" then
    let affinity_nodes = Xml.children node in
    let get_affinity_id n =
      force_tag n "AffinityId";
      int_of_string (get_tag_val n)
    in
    let affinity_ids = Array.of_list (List.map get_affinity_id affinity_nodes)
    in
    {core with thread_affinity_ids = affinity_ids}
  else if test_tag node "Caches" then
    let cache_nodes = Xml.children node in
    let num_caches = (List.length cache_nodes) in
    let caches = Array.make num_caches (-1) in
    let get_cache_id n =
      let id = int_of_string (String.sub (Xml.tag n) 1 1) in
      if id < 1 or id > num_caches then
        failwith ("Unexpected Cache level: " ^ (Xml.tag n));
      Array.set caches (id-1) (int_of_string (get_tag_val n))
    in
    List.iter get_cache_id cache_nodes;
    {core with cache_ids = caches}
  else failwith ("Unexpected Core XML child: " ^ (Xml.tag node))

let build_core (node:Xml.xml) (cpu:cpu_t) =
  let children = Xml.children node in
  let dummy_core = {
	  core_id = -1;
	  cache_ids = Array.make 0 0;
	  thread_affinity_ids = Array.make 0 0
	} in
  List.fold_left consume_core_child dummy_core children

let consume_cpu_child (cpu:cpu_t) (node:Xml.xml) =
  if test_tag node "Id" then
    let id = int_of_string (get_tag_val node) in
    {cpu with cpu_id = id}
  else if test_tag node "Core" then
    let core = build_core node cpu in
    let new_cores = Array.append cpu.cores (Array.make 1 core) in
    {cpu with cores = new_cores}
  else if test_tag node "Cache" then
    let cache = build_cache node cpu in
    let new_caches = Array.append cpu.caches (Array.make 1 cache) in
    {cpu with caches = new_caches}
  else failwith ("Unexpected CPU XML child: " ^ (Xml.tag node))

let build_cpu (node:Xml.xml) =
  match node with
  | Xml.Element _ ->
    if not (test_tag node "Machine") then
      failwith "CPU XML file must start with Machine node.";
    let cpus = Xml.children node in
    (* TODO: for now only support 1 CPU *)
    assert(List.length cpus == 1);
    let cpunode = List.hd cpus in
    let dummy_core = {
      core_id = -1;
      cache_ids = Array.make 0 0;
      thread_affinity_ids = Array.make 0 0
    } in
    let dummy_cache = {
      cache_id = -1;
      level = -1;
      core_ids = Array.make 0 0;
      size_kb = -1;
      line_size_bytes = -1;
      associativity = -1
    } in
    let initialcpu = {
      cpu_id = -1;
      cpu_clock_rate_ghz = 0.0;
      cores = Array.make 0 dummy_core;
      caches = Array.make 0 dummy_cache
    } in
    let cpu_children = Xml.children cpunode in
    let cpu = List.fold_left consume_cpu_child initialcpu cpu_children in
    cpu
  | _ -> failwith "Expected Machine node in CPU XML configuration file"

let build_gpu (node:Xml.xml) =
  {
    gpu_id = -1;
	  name = "GPU0";
	  global_mem = -1;
	  shared_mem_per_sm = -1;
	  regs_per_block = -1;
	  warp_size = -1;
	  mem_pitch = -1;
	  max_threads_per_block = -1;
	  max_threads_per_x = -1;
	  max_threads_per_y = -1;
	  max_threads_per_z = -1;
	  max_grid_size_x = -1;
	  max_grid_size_y = -1;
	  gpu_clock_rate_ghz = 0.0;
	  total_const_mem = 0;
	  accessible_peers = Array.make 0 0;
	  global_mem_id = 0;
	  peak_global_bw = 0.0;
  }

let build_machine_model =
  let homedir = Sys.getenv "HOME" in
  let gpufile = homedir ^ "/.parakeet/parakeetgpuconf.xml" in
  let gpuxml = Xml.parse_file gpufile in
  let gpus = Array.make 1 (build_gpu gpuxml) in
  let cpufile = homedir ^ "/.parakeet/parakeetcpuconf.xml" in
  let cpuxml = Xml.parse_file cpufile in
  let cpus = Array.make 1 (build_cpu cpuxml) in
  IFDEF DEBUG THEN print_cpu cpus.(0); ENDIF;
  {gpus = gpus;cpus = cpus; total_ram = 16384}
