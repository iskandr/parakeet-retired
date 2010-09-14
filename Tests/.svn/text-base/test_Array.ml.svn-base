
open Base

let _ = Test.add_module "Array" [ 
  Test.ok "keep_if"  
    (fun () -> Array.keep_if [|1;2;3;4;5|] (fun x -> x mod 2 = 0) = [|2;4|]);

  Test.ok "remove_if" 
     (fun ()-> Array.remove_if [|10;20;30;40|] (fun x -> x > 20) = [|10;20|]);
     
  Test.ok "keep_mask"  
     (fun () -> 
        Array.keep_mask [|"a";"b";"c"|] [|true; false; true|] = [|"a";"c"|]
     );
    
  Test.ok "remove_mask"  
     (fun () -> 
         Array.remove_mask [|"z";"y";"x"|] [|true; false; false|] = [|"y";"x"|]
     );
    
  Test.ok "min_elt"  (fun () -> Array.min_elt [|3;2;0;1|]  = 0);
    
  Test.ok "max_elt" (fun () -> Array.max_elt [|30;22;-3;100|]  = 100);
]
      