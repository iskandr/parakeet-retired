  let map2 f (x,y) = f x, f y 
  let map3 f (x,y,z) = f x, f y, f z
  let map4 f (x,y,z,w) = f x, f y, f z, f w

  let uncurry2 f (x,y) = f x y
  let uncurry3 f (x,y,z) = f x y z
  let uncurry4 f (x,y,z,w) = f x y z w

  let curry2 f x y = f (x,y)
  let curry3 f x y z = f (x,y,z)
  let curry4 f x y z w = f (x,y,z,w)
  
  let rec zip3 xs ys zs =
    match xs, ys, zs with 
      | [], _, _
      | _, [], _
      | _, _, [] -> []
      | x::xs, y::ys, z::zs -> (x,y,z)::zip3 xs ys zs

  let rec zip xs ys  =
    match xs, ys with 
      | [], _
      | _, [] -> []
      
      | x::xs, y::ys -> (x,y)::zip xs ys
    
  let rec zip4 xs ys zs ws =
    match xs, ys, zs, ws with 
      | [], _, _, _
      | _, [], _, _
      | _, _, [],_ 
      | _, _, _, [] -> []
      | x::xs, y::ys, z::zs, w::ws -> (x,y,z,w)::zip4 xs ys zs ws