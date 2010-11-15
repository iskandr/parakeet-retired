  open BaseCommon
  include ExtArray.Array
  
  let remove_mask data bools = 
    let len = Array.length data in 
    if len = 0 then data else if length bools <> len 
    then failwith "Mask length does not match array"  else
    let result = DynArray.create () in 
    for i = 0 to len -1 do 
      if not $ bools.(i) then DynArray.add result data.(i)
    done; 
    DynArray.to_array result   

  let keep_mask data bools = 
    let len = Array.length data in 
    if len = 0 then data else if Array.length bools <> len 
    then failwith "Mask length does not match array"  else
    let result = DynArray.create () in 
    for i = 0 to len -1 do 
      if bools.(i) then DynArray.add result data.(i)
    done; 
    DynArray.to_array result   


  let keep_if data pred = 
    let len = Array.length data in 
    if len = 0 then data else
    let result = DynArray.create () in 
    for i = 0 to len -1 do 
        if pred data.(i) then DynArray.add result data.(i) 
    done; 
    DynArray.to_array result   
    
  let remove_if data pred = keep_if data (fun x -> not (pred x))
  
  let reduce f arr = 
    let n = Array.length arr in   
    if n = 0 then failwith "Can't reduce empty array"
    else 
      let m = ref arr.(0) in 
      for i = 1 to n - 1 do
        m := f arr.(i) !m
      done; 
      !m 

  let min_elt arr = reduce min arr
  let max_elt arr = reduce max arr
  
  let map2 f x y =     
    let lenx, leny  = length x, length y in
    let minlen =  min lenx leny in
    if minlen = 0 then [||] 
    else 
    let firstElt = f x.(0) y.(0) in 
    let results = create minlen firstElt in 
    for i = 1 to minlen - 1 do 
      results.(i) <- f x.(i) y.(i)
    done; 
    results 
  
  let iter2 f x y =     
    let lenx, leny  = length x, length y in
    assert (lenx = leny);
    for i = 0 to lenx - 1 do 
      f x.(i) y.(i)
    done 
  
  let iter3 f x y z =     
    let lenx, leny, lenz  = length x, length y, length z in
    assert (lenx = leny && leny = lenz);
    for i = 0 to lenx - 1 do 
      f x.(i) y.(i) z.(i)
    done 
  
  let fold_left2 f init x y = 
    let lenx, leny  = length x, length y in
    assert (lenx = leny);
    if lenx = 0 then init 
    else 
    let acc = ref init in (
    for i = 0 to lenx - 1 do 
      acc := f !acc x.(i) y.(i)
    done; 
    !acc
    )
    
  let sum xs = fold_left (+) 0 xs 