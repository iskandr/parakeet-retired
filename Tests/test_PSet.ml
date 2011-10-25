

Test.add_module "PSet" [
    Test.ok "list constructor" (fun () -> 
        let s1 = PSet.add 1 (PSet.add 2 PSet.empty) in 
        let s2 = PSet.from_list [1;2] in 
        PSet.equal s1 s2 
    );
    
   Test.ok "diff"  
    (fun () -> 
      let s1 = PSet.from_list [1;2;3] in 
      let s2 = PSet.from_list [2;3] in 
      PSet.equal (PSet.diff s1 s2) (PSet.singleton 1)
    ); 
      
  Test.bracket "from_list . to_list == id" 
    ~buildup:(fun()-> PSet.add "hello" (PSet.add "goodbye" PSet.empty))
    (fun set ->  
      PSet.equal (PSet.from_list (PSet.to_list set))  set
    );
         
  Test.ok "add . remove == id"   
    (fun () -> 
      PSet.is_empty (PSet.remove 1 (PSet.add 1 PSet.empty)) 
    );
  
  Test.bracket "add x (add x set) == add x set" 
    ~buildup:(fun () -> PSet.add 1 (PSet.add (-1) PSet.empty))
    (fun set -> 
      let s1 = PSet.add 30 (PSet.add 30 set) in 
      let s2 = PSet.add 30 set in 
      PSet.equal s1 s2  
    );
  
  Test.ok "compare disjoint sets"  
    (fun () -> 
      PSet.Disjoint = 
        PSet.compare_sets (PSet.from_list [1;2;3]) (PSet.from_list [4;5]) 
    );  
  
  Test.ok "compare overlapping sets"  
    (fun () -> 
      PSet.Overlap = 
        PSet.compare_sets (PSet.from_list [1;2;3]) (PSet.from_list [3;5]) 
    );  
  
  Test.ok "compare identical sets"  
    (fun () -> 
      PSet.Same = 
        PSet.compare_sets (PSet.from_list [1;2;3]) (PSet.from_list [1;2;3]) 
    );   
] 