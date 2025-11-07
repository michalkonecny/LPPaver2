export type Var = string
  
export type Expr = { e: ExprF<Expr> }

export type ExprF<E> = ExprVar | ExprLit | ExprUnary<E> | ExprBinary<E>

export type ExprVar = {
  tag: 'ExprVar',
  var: Var
}

export type ExprLit = {
  tag: 'ExprLit',
  lit: number
}

export type ExprUnary<E> = {
  tag: 'ExprUnary',
  unop: UnaryOp,
  e1: E
}

export type ExprBinary<E> = {
  tag: 'ExprBinary',
  binop: BinaryOp,
  e1: E,
  e2: E
}

export type UnaryOp = 'OpNeg' | 'OpSqrt' | 'OpSin' | 'OpCos'
export type BinaryOp = 'OpPlus' | 'OpMinus' | 'OpTimes' | 'OpDivide'
