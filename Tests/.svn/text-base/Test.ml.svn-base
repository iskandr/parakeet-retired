open Base
open Printf 

let _ = Printexc.record_backtrace true
type result = 
  | ResultOk  
  | ResultFail  
  | ResultDiag of string
  | ResultAssertFail of string * int * int 
  | ResultExn of string
  | ResultBreakdownExn of string 
  | ResultBuildupExn of string 

type test_type = 
  | OkTest    
  | Diag 

type test = {
  test_type : test_type;
  test_name : string;
  test_wrapper : (unit->result); 
} 

type test_module = string * test array  

let (testModules:test_module DynArray.t) = DynArray.create ()  

let is_result_fail = function 
  | ResultFail 
  | ResultAssertFail _
  | ResultExn _  
  | ResultBuildupExn _ 
  | ResultBreakdownExn _ -> true 
  | _ -> false 

let is_result_diag = function 
  | ResultDiag _ -> true 
  | _ -> false
 
let is_result_ok = function 
  | ResultOk  -> true
  | _ -> false
 
let result_to_str = function 
  | ResultOk -> ""
  | ResultFail -> ""
  | ResultDiag str -> str 
  | ResultAssertFail (file,line,col) -> 
        sprintf "Assert failed: %s on line %d, row %d" file line col
  | ResultExn str -> sprintf "Exception during test: %s" str
  | ResultBuildupExn str ->  sprintf "Exception during buildup: %s" str
  | ResultBreakdownExn str -> sprintf "Exception during breakdown: %s" str
         
let noop () = () 
let ignore _ = () 
  

let ok_wrapper buildup breakdown pred () =   
  try 
    let data = buildup() in
    let result = try if pred data then ResultOk else ResultFail 
                 with exn -> ResultExn (Printexc.to_string exn)
              
    in (try breakdown data; result
        with exn -> ResultBreakdownExn (Printexc.to_string exn))
  with exn -> ResultBuildupExn (Printexc.to_string exn)

let ok name pred = 
  let wrapper = ok_wrapper noop ignore pred  in    
  { test_type = OkTest; test_wrapper = wrapper; test_name = name}  

let bracket name ~buildup  ?(breakdown=ignore) pred = 
  let wrapper = ok_wrapper buildup breakdown pred  in    
  { test_type = OkTest; test_wrapper = wrapper; test_name = name}
      
let diag_wrapper buildup breakdown fn () =   
  try 
    let data = buildup() in
    let result = try ResultDiag (fn data)  
                 with exn -> ResultExn (Printexc.to_string exn)
    in (try breakdown data; result
        with exn -> ResultBreakdownExn (Printexc.to_string exn))
  with exn -> ResultBuildupExn (Printexc.to_string exn)
     
     
let diag name fn =
  let wrapper = diag_wrapper noop ignore fn in 
  { test_type = Diag; test_wrapper = wrapper; test_name = name}  

let diag_bracket name ~buildup ?(breakdown=ignore) fn =
  let wrapper = diag_wrapper buildup breakdown fn in 
  { test_type = Diag; test_wrapper = wrapper; test_name = name}  

let add_module name tests = DynArray.add testModules (name,Array.of_list tests)   
  
let run_tests () =
  let total = ref 0 in 
  let passed = ref 0 in 
  let diags = ref 0 in 
  for i = 0 to DynArray.length testModules - 1 do
    let (moduleName,tests) = DynArray.get testModules i in
    printf "\n======= %s =======\n" moduleName;  
    for j = 0 to Array.length tests - 1 do 
      let test = tests.(j) in  
      let result = test.test_wrapper () in
      if is_result_diag result then 
         diags := !diags + 1
      else (
        total := !total + 1; 
        if is_result_ok result then passed := !passed + 1
      );  
      let resultStr = result_to_str result in
      if is_result_ok result then printf "OK :: "
      else if is_result_diag result then printf  "# " 
      else printf "FAIL :: "; 
      if String.length resultStr = 0 then print_endline test.test_name
      else 
        match test.test_type with 
        | OkTest ->  printf "%s -- %s\n"  test.test_name resultStr;
        | Diag -> 
            let diagStr = snd (String.replace resultStr "\n" "\n#\t") in 
            let len = String.length diagStr in 
            if diagStr.[len-1] = '\n' then diagStr.[len-1]<-' '; 
            printf "[%s] %s\n" test.test_name diagStr
    done 
  done;
  printf "\nSummary: %d/%d tests passed\n\n" !passed  !total   