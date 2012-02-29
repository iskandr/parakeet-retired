let _ = Printexc.record_backtrace true

include ExtList.List

  (* are the elements of the two lists equal? *)
  let rec eq_elts x y =
    let xNil = (x = []) in
    let yNil = (y = []) in
    if xNil then yNil
    else if yNil then xNil
    else if hd x = hd y then eq_elts (tl x) (tl y)
    else false

  let rec fill x = function
    | _::rest -> x::(fill x rest)
    | [] -> []

  (* returns a list of numbers from 0 to upper_bound - 1 *)
  let rec range lowerBound upperBound =
    if lowerBound > upperBound then []
    else lowerBound :: range (lowerBound + 1) upperBound

  let til upperBound = range 0 (upperBound - 1)

  let rev_iter f lst = iter f (rev lst)

  let cons x y = x :: y

  let rec fold_left3 f acc xs ys zs =
    match (xs,ys,zs) with
    | [],_,_
    | _,[],_
    | _,_,[] -> acc
    | x::xs, y::ys, z::zs ->
        let acc' = f acc x y z in
        fold_left3 f acc' xs ys zs

  let rec split3 = function
    | [] -> [], [], []
    | (x,y,z)::rest ->
      let xs, ys, zs = split3 rest in
      x::xs, y::ys, z::zs

  let rec iter3 f xs ys zs =
    match xs, ys, zs with
    | [], [], [] -> ()
    | x::xs, y::ys, z::zs ->
      f x y z;
      iter3 f xs ys zs
    | _ -> failwith "iter3 - mismatched list lengths"

  let rec map3 f xs ys zs =
    match xs,ys,zs with
    | [], [], [] -> []
    | x::xs', y::ys', z::zs' ->
      (f x y z)::(map3 f xs' ys' zs')
    | _ -> failwith "map3 - mismatched list lengths"

  let rec map f = function
    | [] -> []
    | [x] -> [f x]
    | [x; y] ->
      let a = f x in
      let b = f y in
      [a;b]
    | [x; y; z] ->
      let a = f x in
      let b = f y in
      let c = f z in
      [a;b;c]
    | x::y::z::w::rest ->
      let a = f x in
      let b = f y in
      let c = f z in
      let d = f w in
      a::b::c::d::(map f rest)

  let min = function
    | head::tail -> fold_left min head tail
    | [] -> failwith "Can't find min of empty list"

  let max = function
    | head::tail -> fold_left max head tail
    | [] -> failwith "Can't find max of empty list"

  let sum xs = fold_left (+) 0 xs
  let prod xs = fold_left ( * ) 1 xs

  let rec drop n lst =
    if n = 0 then lst else drop (n-1) (tl lst)

  let rec take ?(acc=[]) n lst =
    if n = 0 then rev acc
    else take ~acc:((hd lst)::acc) (n-1) (tl lst)

  (* splits a list into its first nth elemenets and the rest *)
  let rec split_nth n ?(left=[]) right = match (n,right) with
    | (_,[]) -> left, []
    | (0, _) -> List.rev left, right
    | (1, x::xs) -> List.rev (x::left), xs (* slight optimization *)
    | (n, x::xs) -> split_nth (n-1) ~left:(x::left) xs

  let rec unique = function
    | [] -> []
    | x::xs ->
      let xs' = unique xs in
      if List.mem x xs' then xs' else x::xs'

  let rec shorter_than xs n = match xs with
    | [] -> n > 0
    | _::xs' -> if n > 0 then (shorter_than xs' (n-1)) else false
