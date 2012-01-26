type 'a t = 'a array 

let empty  = [||]
let singleton x = [|x|]
let of_list = Array.of_list 
let of_array x = x
  
let append = Array.append
let concat = Array.concat 

let insert_after block stmtNode = concat [block; singleton stmtNode]  
let insert_before stmtNode block = concat [singleton stmtNode; block]
   
let length = Array.length
let idx = Array.get

let iter_forward f block =
  let n = length block in 
  for i = 0 to n - 1 do 
    f (idx block i)
  done

let iter_backward f block = 
  let n = length block in 
  for i = n-1 downto 0 do 
    f (idx block i)
  done

let fold_forward f init block = 
  let acc = ref init in 
  let n = length block in 
  for i = 0 to n-1 do 
    acc := f !acc (idx block i) 
  done; 
  !acc
   
let fold_backward f init block =
  let acc = ref init in
  let n = length block in
  for i = n-1 downto 0 do
    acc := f !acc (idx block i)
  done;
  !acc

let for_all f block =
  let b = ref true in
  let i = ref 0 in
  let n = length block in
  while !b && !i < n do
    let currStmtNode = idx block !i in
    b := !b && (f currStmtNode);
    i := !i + 1;
  done;
  !b

let to_str f block = fold_forward (fun s x -> s ^ "\n" ^ (f x)) "" block
 