open Imp 

  module Lattice = struct 
    type dims = int list 
    type rank = int 
    type t = GlobalVec of rank | SharedVec of dims | Scalar | Top | Bottom  
    let top = Top 
    let bottom = Bottom 
    let join v1 v2 = match v1, v2 with 
    | Bottom, _ -> v2 
    | _, Bottom -> v1
    | _ when v1 = v2 -> v1 
    | _ -> Top 
  end
  open Lattice 
  
  let rec eval_exp latticeEnv = function
  | Var id -> PMap.find id latticeEnv
  | Idx (lhs, rhs) ->
      (match eval_exp latticeEnv lhs with
        | Scalar -> failwith "[imp-infer-memspace] attempted to index scalar"
        | GlobalVec 1 -> Scalar
        | GlobalVec r -> GlobalVec (r - 1)
        | SharedVec [d] -> Scalar
        | SharedVec (d::ds) -> SharedVec ds
        | Top -> failwith "Expression might have multiple Ptx memory spaces"
        | _ -> failwith "Insufficient information in abstract interpreter"
       )
  | Select (_, _, tExp, fExp) ->
      let tSpace = eval_exp latticeEnv tExp in
      let fSpace = eval_exp latticeEnv fExp in
      if tSpace <> fSpace
      then failwith "Branches of select statement must agree on memspace"
      else tSpace
  | _ -> Scalar

