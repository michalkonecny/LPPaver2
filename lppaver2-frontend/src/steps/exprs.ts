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

export type ExprHash = string
export type ExprDict = Record<string, ExprF<ExprHash>>

export function exprHashToExpr(exprHash: ExprHash, dict: ExprDict): Expr {
  const exprF = dict[exprHash];
  if (!exprF) {
    throw new Error(`Expression with hash ${exprHash} not found in dict`);
  }

  switch (exprF.tag) {
    case 'ExprVar':
      return { e: exprF };
    case 'ExprLit':
      return { e: exprF };
    case 'ExprUnary':
      return {
        e: {
          tag: 'ExprUnary',
          unop: exprF.unop,
          e1: exprHashToExpr(exprF.e1, dict)
        }
      };
    case 'ExprBinary':
      return {
        e: {
          tag: 'ExprBinary',
          binop: exprF.binop,
          e1: exprHashToExpr(exprF.e1, dict),
          e2: exprHashToExpr(exprF.e2, dict)
        }
      };
    default:
      throw new Error(`Unknown expression tag: ${(exprF as any).tag}`);
  }
}