import type { Expr, Var } from "./exprs";

export function evalFPExpr(expr: Expr, env: Record<Var, number>): number {
  switch (expr.e.tag) {
    case "ExprLit":
      const rational = expr.e.lit;
      return rational.numerator / rational.denominator;
    case "ExprVar":
      return env[expr.e.var] ?? 0;
    case "ExprUnary":
      {
        const val1 = evalFPExpr(expr.e.e1, env);
        switch (expr.e.unop) {
          case "OpNeg": return -val1;
          case "OpCos": return Math.cos(val1);
          case "OpSin": return Math.sin(val1);
          case "OpSqrt": return Math.sqrt(val1);
        }
      }
    case "ExprBinary":
      const val1 = evalFPExpr(expr.e.e1, env);
      const val2 = evalFPExpr(expr.e.e2, env);
      switch (expr.e.binop) {
        case "OpPlus": return val1 + val2;
        case "OpMinus": return val1 - val2;
        case "OpTimes": return val1 * val2;
        case "OpDivide": return val1 / val2;
      }
  }
}