open Base 

let timers : string  list ref = ref []  

let startTimes : (string, float) Hashtbl.t = Hashtbl.create 127
(* accumulated time *) 
let accTimes : (string, float) Hashtbl.t = Hashtbl.create 127 
  
let get_time () = Unix.gettimeofday ()

let reset_timer name =
  if not (List.mem name !timers) then timers := name :: !timers;  
  Hashtbl.remove startTimes name; 
  Hashtbl.replace accTimes name 0.0

let start_timer name = 
  let currTime = get_time() in 
(* if not yet initialized, then initialize *) 
  if not (Hashtbl.mem accTimes name) then (
    timers := name :: !timers; 
    Hashtbl.add accTimes name 0.0 
  );
  Hashtbl.replace startTimes name currTime 
  
let stop_timer name =
  if Hashtbl.mem startTimes name then ( 
    let currTime = get_time() in
    let startTime = Hashtbl.find startTimes name in 
    let extra = currTime -. startTime in
    let accTime = Hashtbl.find accTimes name in
    Hashtbl.replace accTimes name (accTime +. extra);
    Hashtbl.remove startTimes name
  ) 


let stop_all () = List.iter stop_timer !timers  
  
let get_total name =
  let accTime = try Hashtbl.find accTimes name with 
    | _ -> failwith (name ^ " timer was never initialized ")
  in 
  (* is the timer still running? *)   
  let extraTime = 
    if Hashtbl.mem startTimes name then 
      get_time() -. (Hashtbl.find startTimes name)
    else 0.0 
  in 
  accTime +. extraTime 
       
  
let print_timers () =
  stop_all (); 
  let print name = 
    Printf.printf "%s: %f\n" name (get_total name) 
  in 
  List.iter print !timers;
  Pervasives.flush_all()  