import { intervalsOverlap, type Interval } from "./evalInfo";
import type { Expr, Var } from "./exprs";
import type { BinaryComp, Form } from "./forms";
import {
  kleeneanAnd,
  kleeneanFromBoolean,
  kleeneanNot,
  kleeneanOr,
  type Kleenean,
} from "./kleenean";

export type Env = Record<Var, number>;

export function evalFPExpr(expr: Expr, env: Env): number {
  switch (expr.e.tag) {
    case "ExprLit":
      const rational = expr.e.lit;
      return rational.numerator / rational.denominator;
    case "ExprVar":
      return env[expr.e.var] ?? 0;
    case "ExprUnary": {
      const val1 = evalFPExpr(expr.e.e1, env);
      switch (expr.e.unop) {
        case "OpNeg":
          return -val1;
        case "OpCos":
          return Math.cos(val1);
        case "OpSin":
          return Math.sin(val1);
        case "OpSqrt":
          return Math.sqrt(val1);
      }
    }
    case "ExprBinary":
      const val1 = evalFPExpr(expr.e.e1, env);
      const val2 = evalFPExpr(expr.e.e2, env);
      switch (expr.e.binop) {
        case "OpPlus":
          return val1 + val2;
        case "OpMinus":
          return val1 - val2;
        case "OpTimes":
          return val1 * val2;
        case "OpDivide":
          return val1 / val2;
      }
  }
}

function almostEqual(a: number, b: number, epsilon = 1e-8): boolean {
  return Math.abs(a - b) < epsilon;
}

function evalFPComp(
  comp: BinaryComp,
  val1: number,
  val2: number,
): boolean {
  switch (comp) {
    case "CompEq":
      return almostEqual(val1, val2);
    case "CompNeq":
      return !almostEqual(val1, val2);
    case "CompLe":
      return val1 < val2 || !almostEqual(val1, val2);
    case "CompLeq":
      return val1 < val2 || almostEqual(val1, val2);
  }
}

export function evalIntervalComp(
  comp: BinaryComp,
  val1: Interval<number>,
  val2: Interval<number>,
): Kleenean {
  switch (comp) {
    case "CompEq":
      return intervalsOverlap(val1, val2) ? "TrueOrFalse" : "CertainFalse";
    case "CompNeq":
      return intervalsOverlap(val1, val2) ? "TrueOrFalse" : "CertainTrue";
    case "CompLe":
      return val1.u < val2.l
        ? "CertainTrue"
        : val1.l > val2.u
          ? "CertainFalse"
          : "TrueOrFalse";
    case "CompLeq":
      return val1.u <= val2.l
        ? "CertainTrue"
        : val1.l > val2.u
          ? "CertainFalse"
          : "TrueOrFalse";
  }
}

export function evalFPForm(form: Form, env: Env): Kleenean {
  switch (form.f.tag) {
    case "FormComp": {
      const val1 = evalFPExpr(form.f.e1, env);
      const val2 = evalFPExpr(form.f.e2, env);
      return kleeneanFromBoolean(evalFPComp(form.f.comp, val1, val2));
    }
    case "FormTrue":
      return "CertainTrue";
    case "FormFalse":
      return "CertainFalse";
    case "FormUnary": {
      const val1 = evalFPForm(form.f.f1, env);
      switch (form.f.uconn) {
        case "ConnNeg":
          return kleeneanNot(val1);
      }
    }
    case "FormBinary":
      const val1 = evalFPForm(form.f.f1, env);
      const val2 = evalFPForm(form.f.f2, env);
      switch (form.f.bconn) {
        case "ConnAnd":
          return kleeneanAnd(val1, val2);
        case "ConnOr":
          return kleeneanOr(val1, val2);
        case "ConnImpl":
          return kleeneanOr(kleeneanNot(val1), val2);
      }
    case "FormIfThenElse":
      const condVal = evalFPForm(form.f.fc, env);
      if (condVal === "CertainTrue") {
        return evalFPForm(form.f.ft, env);
      } else if (condVal === "CertainFalse") {
        return evalFPForm(form.f.ff, env);
      } else {
        const thenVal = evalFPForm(form.f.ft, env);
        const elseVal = evalFPForm(form.f.ff, env);
        if (thenVal === elseVal) {
          return thenVal;
        } else {
          return "TrueOrFalse";
        }
      }
  }
}
