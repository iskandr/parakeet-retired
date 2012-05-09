/*
 *  ast_variants.h
 *
 *  Enumerations that represent the numerical values of the OCaml Variant types
 *  we use in the C interface for the AST.ml types.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */

#ifndef AST_VARIANTS_H_
#define AST_VARIANTS_H_

enum ast_exp_no_data {
  Exp_Void = 0
};
enum ast_exp_data {
  Exp_Var,
  Exp_Prim,
  Exp_Num,
  Exp_Str,
  Exp_Type,
  Exp_None, 
  Exp_Array,
  Exp_Tuple,
  Exp_Lambda,
  Exp_Call,
  Exp_Assign,
  Exp_Return,
  Exp_Block,
  Exp_If,
  Exp_WhileLoop,
  Exp_CountLoop, 
};

#endif
