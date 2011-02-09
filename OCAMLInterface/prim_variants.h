/*
 *  prim_variants.h
 *
 *  Enumerations that represent the numerical values of the OCaml Variant types
 *  we use in the C interface for the Prim.ml types.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2011.
 */

#ifndef PRIM_VARIANTS_H_
#define PRIM_VARIANTS_H_

/** For Prim.ml **/
enum scalar_op {
  Scalar_Op_Add = 0,
  Scalar_Op_Sub,
  Scalar_Op_Mult,
  Scalar_Op_SafeDiv,
  Scalar_Op_Div,
  Scalar_Op_Pow,
  Scalar_Op_Log,
  Scalar_Op_Mod,
  Scalar_Op_Min,
  Scalar_Op_Max,
  Scalar_Op_Eq,
  Scalar_Op_Neq,
  Scalar_Op_Lt,
  Scalar_Op_Lte,
  Scalar_Op_Gt,
  Scalar_Op_Gte,
  Scalar_Op_And,
  Scalar_Op_Or,
  Scalar_Op_Not,
  Scalar_Op_Neg,
  Scalar_Op_Sqrt,
  Scalar_Op_Sign,
  Scalar_Op_Reciprocal,
  Scalar_Op_Abs,
  Scalar_Op_Ceil,
  Scalar_Op_Floor,
  Scalar_Op_Round,
  Scalar_Op_Exp,
  Scalar_Op_Exp2,
  Scalar_Op_Sin,
  Scalar_Op_Cos,
  Scalar_Op_Tan,
  Scalar_Op_Sinh,
  Scalar_Op_Cosh,
  Scalar_Op_Tanh,
  Scalar_Op_Ln,
  Scalar_Op_Lg2,
  Scalar_Op_Log10,
  Scalar_Op_Select
};

enum array_op {
  Array_Op_Zip = 0,
  Array_Op_Concat,
  Array_Op_Enlist,
  Array_Op_Til,
  Array_Op_Rand,
  Array_Op_Index,
  Array_Op_Where,
  Array_Op_Find,
  Array_Op_DimSize
};

enum adverb {
  Adverb_Map = 0,
  Adverb_EachLeft,
  Adverb_EachRight,
  Adverb_Scan,
  Adverb_Reduce,
  Adverb_AllPairs,
  Adverb_AllPairsRight
};

enum impure_op {
  Impure_Op_ResetTimer = 0,
  Impure_Op_GetTimer,
  Impure_Op_Print
};

enum q_op {
  Q_Op_Q_WriteOrLoadText = 0,
  Q_Op_Q_WriteOrLoadBinary,
  Q_Op_Q_Question,
  Q_Op_Q_Dollar
};

enum prim {
  Prim_ScalarOp = 0,
  Prim_ArrayOp,
  Prim_Adverb,
  Prim_ImpureOp,
  Prim_Q_Op
};

#endif
