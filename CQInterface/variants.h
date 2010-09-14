/*
 *  variants.h
 *
 *  Enumerations that represent the numerical values of the OCaml Variant types
 *  we use in the C interface.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2010.
 */

/**
  * Needs to be updated if the types change!
  *
  * I put the non-data Variants first for each type, then the data versions
 **/
enum dyn_type_no_data {
  UnitT = 0,
  CharT,
  Int32T,
  Int64T,
  IntT,
  BoolT,
  Float32T,
  Float64T,
  SymT,
  StrT,
  AnyT
};
enum dyn_type_data {
  VecT = 0,
  TupleT,
  TableT
};


