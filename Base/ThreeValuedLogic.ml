
(* Saturating 3VL, where No takes precedence over all other values, 
   Yes takes precedence over Maybe.
  
   Equivalent to Set = {0=Maybe,1=Yes,2=No}, 
   combine = +, saturates at 2.  
*) 
type t = Yes | Maybe | No 

let combine x y = match x,y with 
  | No, _  
  | _, No  -> No 
  | Yes, _ 
  | _, Yes -> Yes
  | Maybe, Maybe -> Maybe 

