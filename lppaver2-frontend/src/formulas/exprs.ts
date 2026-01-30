export type Var = string

export type Expr = { 
  e: ExprF<Expr>,
  hash: ExprHash
}

export type ExprF<E> = ExprVar | ExprLit | ExprUnary<E> | ExprBinary<E>

export type ExprVar = {
  tag: 'ExprVar',
  var: Var
}

export type RationalLit = {
  numerator: number,
  denominator: number
}

export type ExprLit = {
  tag: 'ExprLit',
  lit: RationalLit
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

export const unaryOpSymbolMap: Record<UnaryOp, string> = {
  'OpNeg': '-',
  'OpSqrt': '√',
  'OpSin': 'sin',
  'OpCos': 'cos'
}

export const binaryOpSymbolMap: Record<BinaryOp, string> = {
  'OpPlus': '+',
  'OpMinus': '-',
  'OpTimes': '*',
  'OpDivide': '/'
}

export type ExprHash = string
export type ExprDict = Record<ExprHash, ExprF<ExprHash>>

export function exprHashToExpr(exprHash: ExprHash, dict: ExprDict): Expr {
  const exprF = dict[exprHash];
  if (!exprF) {
    throw new Error(`Expression with hash ${exprHash} not found in dict`);
  }

  switch (exprF.tag) {
    case 'ExprVar':
      return { e: exprF, hash: exprHash };
    case 'ExprLit':
      return { e: exprF, hash: exprHash };
    case 'ExprUnary':
      return {
        e: {
          tag: 'ExprUnary',
          unop: exprF.unop,
          e1: exprHashToExpr(exprF.e1, dict)
        },
        hash: exprHash
      };
    case 'ExprBinary':
      return {
        e: {
          tag: 'ExprBinary',
          binop: exprF.binop,
          e1: exprHashToExpr(exprF.e1, dict),
          e2: exprHashToExpr(exprF.e2, dict)
        },
        hash: exprHash
      };
    default:
      throw new Error(`Unknown expression tag: ${(exprF as any).tag}`);
  }
}