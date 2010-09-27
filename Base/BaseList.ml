include ExtList.List

  let rec fill x = function 
    | _::rest -> x::(fill x rest)
    | [] -> []


  (* returns a list of numbers from 0 to upper_bound - 1 *)
  let til upperBound =
    let rec aux acc x =
      if x <= 0 then acc
        else let x' = x-1 in aux (x'::acc) x'
    in aux [] upperBound

  let rev_iter f lst = iter f (rev lst)

  let cons x y = x :: y
  
  let rec fold_left3 f acc xs ys zs = 
    match (xs,ys,zs) with 
      | [],_,_
      | _,[],_
      | _,_,[] -> acc
      |x::xs, y::ys, z::zs -> 
         let acc' = f acc x y z in 
         fold_left3 f acc' xs ys zs 
        
  let rec split3 = function 
  | [] -> [], [], [] 
  | (x,y,z)::rest -> 
      let xs, ys, zs = split3 rest in 
      x::xs, y::ys, z::zs
      
  let rec map3 f xs ys zs = 
    match xs,ys,zs with 
      | [], [], [] -> [] 
      | x::xs', y::ys', z::zs' ->
        (f x y z)::(map3 f xs' ys' zs') 
      | _ -> failwith "map3 - mismatched list lengths" 