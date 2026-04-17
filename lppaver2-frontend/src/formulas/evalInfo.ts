import _ from "lodash";
import type { Expr, ExprHash } from "./exprs";
import type { FormHash } from "./forms";
import type { Kleenean } from "./kleenean";
import type { Triangulation2D } from "./mesh";

export type EvalInfo = {
  formValues: FormValues;
  exprValues?: ExprValues;
};

export type FormValues = Record<FormHash, Kleenean>;
export type ExprValues = Record<ExprHash, ExprValue>;

export type ExprValue = Interval<number> | AffineForm;

export type Interval<T> = {
  l: T;
  u: T;
};

export function intervalWidth(interval: Interval<number>): number {
  return interval.u - interval.l;
}

export type AffineForm = {
  center: number;
  errTerms: Record<string, number>;
};

export function exprValueIsInterval(
  exprValue: ExprValue,
): exprValue is Interval<number> {
  return "l" in exprValue && "u" in exprValue;
}

export function exprValueIsAffineForm(
  exprValue: ExprValue,
): exprValue is AffineForm {
  return "center" in exprValue && "errTerms" in exprValue;
}

// export function evalAffineForm(aff: AffineForm, x: number[], y: number[]): { l: number[], u: number[] } {
// }

export function evalExprOnTriangulation(
  eValue: ExprValue | undefined,
  xExprValue: ExprValue | undefined,
  yExprValue: ExprValue | undefined,
  xVarDomain: Interval<number> | undefined,
  yVarDomain: Interval<number> | undefined,
  triang: Triangulation2D | undefined,
): { l: number[]; u: number[] } | undefined {
  if (!eValue) return undefined;
  if (!triang) return undefined;

  if (exprValueIsInterval(eValue)) {
    const l: number[] = Array(triang.x.length).fill(eValue.l);
    const u: number[] = Array(triang.x.length).fill(eValue.u);
    return { l, u };
  }

  if (exprValueIsAffineForm(eValue)) {
    // If expr value is an affine form, also the value for x and y will be affine forms.
    // The affine form for a variable will have a single error term, whose key is the variable's ID.
    const xErrId =
      xExprValue && exprValueIsAffineForm(xExprValue)
        ? _.keys(xExprValue.errTerms)[0]
        : undefined;
    const yErrId =
      yExprValue && exprValueIsAffineForm(yExprValue)
        ? _.keys(yExprValue.errTerms)[0]
        : undefined;

    const xErrCoeff: number = xErrId ? (eValue.errTerms[xErrId] ?? 0) : 0;
    const yErrCoeff: number = yErrId ? (eValue.errTerms[yErrId] ?? 0) : 0;
    const otherErrCoeffs: number[] = Object.entries(eValue.errTerms)
      .filter(([errId, _]) => errId !== xErrId && errId !== yErrId)
      .map(([_, coeff]) => coeff);
    const otherErrSum = _.sum(otherErrCoeffs.map(Math.abs));

    function xToUnit(x?: number): number {
      const xDom = xVarDomain;
      if (!xDom) return 0;
      const c = (xDom.l + xDom.u) / 2;
      return (2 * ((x ?? c) - c)) / (xDom.u - xDom.l);
    }

    function yToUnit(y?: number): number {
      const yDom = yVarDomain;
      if (!yDom) return 0;
      const c = (yDom.l + yDom.u) / 2;
      return (2 * ((y ?? c) - c)) / (yDom.u - yDom.l);
    }

    const c = triang.x.map(
      (_, i) =>
        eValue.center +
        xErrCoeff * xToUnit(triang.x[i]) +
        yErrCoeff * yToUnit(triang.y[i]),
    );
    const l = c.map((v) => v - otherErrSum);
    const u = c.map((v) => v + otherErrSum);
    return { l, u };
  }

  return undefined;
}
