type 'a t = 'a DynArray.t

let create () = DynArray.create()

let singleton x =
  let b = DynArray.make 1 in
  DynArray.add b x;
  b

let of_list xs = DynArray.of_list xs
let of_array xs = DynArray.of_array xs

let append b1 b2 =
  let b1' = DynArray.copy b1 in
  (* put b2 at the end of b1 *)
  DynArray.append b2 b1';
  b1'

let add b x = DynArray.add b x

let length b = DynArray.length b
let idx b i = DynArray.get b i

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

let exists f block =
  let b = ref false in
  let i = ref 0 in
  let n = length block in
  while not !b && !i < n do
    let currStmtNode = idx block !i in
    b := f currStmtNode;
    i := !i + 1
  done;
  !b

let find_first f block =
  let result = ref None in
  let i = ref 0 in
  let n = length block in
  while !result = None && !i < n do
    result := f (idx block !i);
    i := !i + 1
  done;
  !result

let to_str f block = fold_forward (fun s x -> s ^ "\n" ^ (f x)) "" block
