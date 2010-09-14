open CodeTemplate
open DynType

type ktype =
  | KBoolT
  | KByteT
  | KShortT
  | KIntT
  | KLongT
  | KRealT
  | KFloatT
  | KCharT
  | KSymbolT
  | KTimetampT
  | KMonthT
  | KDateT
  | KDatetimeT
  | KTimespanT
  | KMinuteT
  | KSecondT
  | KTimeT
  | KTableT
  | KDictT
  | KErrorT
  (* The types below here don't actually exist in K itself *)
  | KVecT of ktype

exception UnsupportedKType
let ktype_to_ocaml_type = function
  | KBoolT -> BoolT
(*  | KByteT -> *)
(*  | KShortT -> *)
  | KIntT -> Int32T
  | KLongT -> Int64T
(*  | KRealT -> *)
  | KFloatT -> FloatT
(*  | KCharT -> *)
  | KSymbolT -> SymT
(*  | KTimetampT -> *)
(*  | KMonthT -> *)
(*  | KDateT -> *)
(*  | KDatetimeT -> *)
(*  | KTimespanT -> *)
(*  | KMinuteT -> *)
(*  | KSecondT -> *)
(*  | KTimeT -> *)
  | _ -> raise UnsupportedKType

exception UnsupportedTypeSize
let sizeof_ktype = function
  | KBoolT
  | KByteT
  | KCharT -> 1
  | KShortT -> 2
  | KIntT
  | KRealT
  | KMonthT
  | KDateT
  | KMinuteT
  | KSecondT
  | KTimeT -> 4
  | KLongT
  | KFloatT
  | KDatetimeT -> 8
  | KSymbolT -> 8 (* Size of a pointer on 64-bit system.. *)
  | _ -> raise UnsupportedTypeSize

exception UnknownTypeNum
let ktypenum_to_ktype = function
  | 1  -> KBoolT
  | 4  -> KByteT
  | 5  -> KShortT
  | 6  -> KIntT
  | 7  -> KLongT
  | 8  -> KRealT
  | 9  -> KFloatT
  | 10 -> KCharT
  | 11 -> KSymbolT
  | 13 -> KMonthT
  | 14 -> KDateT
  | 15 -> KDatetimeT
  | 17 -> KMinuteT
  | 18 -> KSecondT
  | 19 -> KTimeT
  | 98 -> KTableT
  | 99 -> KDictT
  | _  -> raise UnknownTypeNum

exception UnknownKVersionOfDynType
let rec dyn_type_to_ktypenum = function
  | BoolT  -> -1
  | Int32T -> -6
  | Int64T -> -7
  | FloatT -> -9
  | SymT   -> -11
  | VecT(vec_type) -> - dyn_type_to_ktypenum vec_type
  | _ -> raise UnknownKVersionOfDynType

let ktypenum_to_ocaml_type ktypenum =
  ktype_to_ocaml_type (ktypenum_to_ktype ktypenum)

type func_desc = string * string list * string list * string

(* Main interface functions *)
let _ = Callback.register "gen_module_template" gen_module_template
let _ = Callback.register "get_function_template" get_function_template
let _ = Callback.register "run_template" run_template

(* Support functions *)
let _ = Callback.register "ktypenum_to_ocaml_type" ktypenum_to_ocaml_type
let _ = Callback.register "dyn_type_to_ktypenum" dyn_type_to_ktypenum
