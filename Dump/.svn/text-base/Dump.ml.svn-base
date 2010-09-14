(*
 * Dump.ml
 *
 * Functions for reading in dumped variable files for feeding runs of the
 * compiler.
 *
 * (c) 2010 Eric Hielscher, Alex Rubinsteyn
 *)
open HostVal

external read_dump_file : string -> (string * host_val) list = "read_dump_file"

let parse_dump_file filename =
  let vars = PMap.empty in
  let vars_list = read_dump_file filename in
  List.fold_left (fun map (a,b) -> PMap.add a b map) vars vars_list

