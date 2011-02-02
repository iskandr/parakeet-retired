let _ = Printexc.record_backtrace true 

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
  
  
  let rec map f = function 
    | [] -> [] 
    | [x] -> [f x] 
    | [x; y] -> [f x; f y]
    | [x; y; z] -> [f x; f y; f z]
    | [x; y; z; w] -> [f x; f y; f z; f w]
    | [x; y; z; w; a] -> [f x; f y; f z; f w; f a]
    | x::y::z::w::a::b::rest -> 
        (f x)::(f y)::(f z)::(f w)::(f a)::(f b)::(map f rest)
  
  let sum xs = fold_left (+) 0 xs 
  
  let rec drop n lst = 
    if n = 0 then lst else drop (n-1) (tl lst)
    
  let rec take ?(acc=[]) n lst = 
    if n = 0 then rev acc
    else take ~acc:((hd lst)::acc) (n-1) (tl lst)
    
  (* splits a list into its first nth elemenets and the rest *) 
  let rec split_nth n ?(left=[])  right = match (n,right) with  
    | (_,[]) -> left, [] 
    | (0, _) -> List.rev left, right
    | (1, x::xs) -> List.rev (x::left), xs  (* slight optimization *)   
    | (n, x::xs) -> split_nth (n-1) ~left:(x::left) xs  
  