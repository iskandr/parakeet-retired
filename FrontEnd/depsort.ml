module StringSet = Set.Make(String)
module StringMap = Map.Make(String)


(* CMX format and loading copied from OCaml 3.12 compiler source. *)
    (* SUPER DANGEROUS: *)
    (*   messed with "value_approximation" to remove dependency on Clambda *)
    
module CompilerInternals = struct 
    type value_approximation =
        Value_closure of int * value_approximation
        | Value_tuple of value_approximation array
        | Value_unknown
        | Value_integer of int
        | Value_constptr of int 

    type cmx_unit_info =
    { 
      mutable ui_name: string;              (* Name of unit implemented *)
      mutable ui_symbol: string;            (* Prefix for symbols *)
      mutable ui_defines: string list;      (* Unit and sub-units implemented *)
      mutable ui_imports_cmi: (string * Digest.t) list; (* Interfaces imported *)
      mutable ui_imports_cmx: (string * Digest.t) list; (* Infos imported *)
      mutable ui_approx: int; (* Approx of the structure *)
      mutable ui_curry_fun: int list;             (* Currying functions needed *)
      mutable ui_apply_fun: int list;             (* Apply functions needed *)
      mutable ui_send_fun: int list;              (* Send functions needed *)
      mutable ui_force_link: bool }               (* Always linked *)
    
    let cmx_magic_number = "Caml1999Y011"
    let read_cmx filename =
        let ic = open_in_bin filename in
        try
            let buffer = String.create (String.length cmx_magic_number) in
            really_input ic buffer 0 (String.length cmx_magic_number);
            if buffer <> cmx_magic_number then begin
                close_in ic;
                failwith "bad cmx file format"
            end;
            let ui = (input_value ic : cmx_unit_info) in
            close_in ic;
            ui
        with End_of_file | Failure _ ->
            close_in ic;
            failwith "error reading file"
            
    let cmo_magic_number = "Caml1999O007"
                            
   type ident = { stamp: int; name: string; mutable flags: int }
    type constant = Const_int of int
    | Const_char of char
    | Const_string of string
    | Const_float of string
    | Const_int32 of int32
    | Const_int64 of int64
    | Const_nativeint of nativeint
  
    type structured_constant = Const_base of constant
    | Const_pointer of int
    | Const_block of int * structured_constant list
    | Const_float_array of string list
    | Const_immstring of string


    type reloc_info = Reloc_literal of structured_constant    (* structured constant *)
    | Reloc_getglobal of ident              (* reference to a global *)
    | Reloc_setglobal of ident              (* definition of a global *)
    | Reloc_primitive of string               (* C primitive number *)
 
    
(* Descriptor for compilation units *)

    type cmo_unit_info = { 
      cu_name: string;                    (* Name of compilation unit *)
      mutable cu_pos: int;                (* Absolute position in file *)
      cu_codesize: int;                   (* Size of code block *)
      cu_reloc: (reloc_info * int) list;  (* Relocation information *)
      cu_imports: (string * Digest.t) list; (* Names and CRC of intfs imported *)
      cu_primitives: string list;         (* Primitives declared inside *)
      mutable cu_force_link: bool;        (* Must be linked even if unref'ed *)
      mutable cu_debug: int;              (* Position of debugging info, or 0 *)
      cu_debugsize: int }                 (* Length of debugging info *)
                                                
    let read_cmo filename = 
        let ic = open_in_bin filename in
        try
            let buffer = String.create (String.length cmo_magic_number) in
            really_input ic buffer 0 (String.length cmo_magic_number);
            if buffer <> cmo_magic_number then begin
                close_in ic;
                failwith ("bad cmo file format, magic number = " ^ buffer)
                 
            end;
            Printf.printf "Preparing to seek...\n";
            let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
            Printf.printf "Seeking to %d\n" compunit_pos; 
            seek_in ic compunit_pos;
            let ui = (input_value ic : cmo_unit_info) in
            close_in ic;
            ui
        with 
          | End_of_file -> 
            close_in ic; 
            failwith "unexpected end of file"
          
          | Failure msg ->
            close_in ic;
            failwith ("error reading file" ^ filename ^ ": " ^ msg)
     
end

type module_info = { filename: string; module_name: string; imports: string list }

let module_info_to_str info : string = 
  Printf.sprintf "%s (%s)" 
    info.module_name
    (String.concat ", " info.imports)
      
let get_cmx_info filename = 
  let info = CompilerInternals.read_cmx filename in 
  { 
    filename = filename; 
    module_name  = info.CompilerInternals.ui_name;
    imports = List.map fst info.CompilerInternals.ui_imports_cmx;
  }
  
let get_cmo_info filename = 
  let info = CompilerInternals.read_cmo filename in 
  { 
    filename = filename; 
    module_name  = info.CompilerInternals.cu_name;
    imports = List.map fst info.CompilerInternals.cu_imports;
  }
  
let get_module_info filename = 
  if Filename.check_suffix filename "cmo" then get_cmo_info filename 
  else get_cmx_info filename 

let check_dir name = 
    try Sys.is_directory name 
    with _ -> 
      (Printf.eprintf "Directory not found: %s" name; false)

let rec set_from_list = function 
  | [] -> StringSet.empty 
  | x::xs -> StringSet.add x (set_from_list xs) 

let dir_contents dirName = 
  let files : string array = Sys.readdir dirName in 
  Array.to_list 
    (Array.map (fun basename -> Filename.concat dirName basename) files)

let topsort moduleInfoList : string list =
  let moduleNames = List.map (fun info -> info.module_name) moduleInfoList in
  let moduleSet = set_from_list moduleNames in
  (* make moduleNames list unique*) 
  (* initialize dependency graph and ready queue *) 
  let successors = Hashtbl.create 127 in
  let degrees = Hashtbl.create 127 in
  let ready = Queue.create() in  
  let sorted : string list ref = ref [] in
  let have_module x = StringSet.mem x moduleSet in
  let add_degree name amt =
    let oldDegree = try Hashtbl.find degrees name with _ -> 0 in 
    Hashtbl.replace degrees name (oldDegree+amt); 
  in 
  let add_edge pred succ = 
    add_degree succ 1; 
    let oldSuccSet = 
      try Hashtbl.find successors pred with _ -> StringSet.empty
    in 
    Hashtbl.replace successors pred (StringSet.add succ oldSuccSet)
  in  
  let insert_module info = 
    (* only add succesors which are actually in our graph *) 
    let imports = List.filter have_module info.imports in
    if imports = [] then 
      Queue.add info.module_name ready
    else 
      List.iter (fun pred -> add_edge pred info.module_name) imports
  in 
  List.iter insert_module moduleInfoList;
  
  let remove_module (name : string) =
    sorted := name :: !sorted;  
    let succs = try Hashtbl.find successors name with _ -> StringSet.empty in
    StringSet.iter 
      (fun succ -> 
        add_degree succ (-1);
        if Hashtbl.find degrees succ = 0 then Queue.add succ ready 
      )
      succs 
  in      
  while not (Queue.is_empty ready) do 
    remove_module (Queue.pop ready)
  done;
  List.rev !sorted 
    
(* inefficient O(n^2) algorithm to filter duplicates *) 
let rec unique_modules = function
  | [] -> []
  | hd :: tl ->
      if List.filter (fun m -> m.module_name = hd.module_name) tl <> [] then 
        unique_modules tl
      else hd :: unique_modules tl  



      
let main () =
  (* start by only searching the current directory *) 
  let searchDirs = ref [] in
  let add_dir name = searchDirs := name :: !searchDirs in
  let recursive = ref false in
  let fullpath = ref false in  
  let excludeSet = ref StringSet.empty in
  let suffix = ref "cmx" in  
  let opts = [ 
    (
      "-E",
      Arg.String (fun s -> excludeSet := StringSet.add s !excludeSet; Printf.eprintf "Excluding %s\n" s),
      "files to exclude"
    ); 
    (
      "-P", 
      Arg.Unit (fun() -> fullpath := true), 
      "include .cmx file path in output"
    );
    ( 
      "-R", 
      Arg.Unit (fun () -> recursive := true), 
      "search for directories recursively"
    );
    ( 
      "-I",
      Arg.String add_dir, 
      "add a directory to the search path"
    ); 
    (
      "-S", 
      Arg.String (fun s -> suffix := s), 
      "suffix of files to search for"
    )
  ] in 
  if !recursive then failwith "Recursive dir search not yet implemented";
  Arg.parse opts (fun _ -> ()) "Usage: depsort <options>";
  (* if user doesn't specify any dirs, assume the current *) 
  if !searchDirs = [] then searchDirs := ["."];  
  let filteredSearchDirs = List.filter check_dir !searchDirs in 
  let allFiles = List.concat (List.map dir_contents filteredSearchDirs) in    
  let moduleFiles = 
    List.filter 
      (fun s -> 
           Filename.check_suffix s !suffix  &&
           Filename.basename s <> "myocamlbuild.cmx" &&  
           not (StringSet.mem s !excludeSet) && 
           not (StringSet.mem (Filename.basename s) !excludeSet)
      ) 
      allFiles
  in 
  (*  modules with duplicate names will get dropped *) 
  let moduleInfoList = unique_modules (List.map get_module_info moduleFiles) in
  
  let sortedModules = topsort moduleInfoList in
  (* mapping between module names and cmx filenames *) 
  let filenameLookup = 
    List.fold_left 
      (fun map info -> 
          let filename = 
            if !fullpath then info.filename else Filename.basename info.filename 
          in   
          StringMap.add info.module_name filename map
      )
      StringMap.empty 
      moduleInfoList    
  in  
  let sortedFiles = 
    List.map (fun name -> StringMap.find name filenameLookup) sortedModules 
  in
  print_string (String.concat " "  sortedFiles)
   

let _ = main () 
