import type { ExprHash } from "./exprs"
import type { FormHash } from "./forms"
import type { Kleenean } from "./kleenean"

export type EvalInfo = {
  formValues: FormValues,
  exprValues?: ExprValues
}

export type FormValues = Record<FormHash, Kleenean>
export type ExprValues = Record<ExprHash, ExprValue>

export type ExprValue = Interval<number> | AffineForm

export type Interval<T> = {
  l: T,
  u: T
}

export function intervalWidth(interval: Interval<number>): number {
  return interval.u - interval.l;
}

export type AffineForm = {
  center: number,
  errTerms: Record<string, number>,
}

export function exprValueIsInterval(exprValue: ExprValue): exprValue is Interval<number> {
  return 'l' in exprValue && 'u' in exprValue;
}

export function exprValueIsAffineForm(exprValue: ExprValue): exprValue is AffineForm {
  return 'center' in exprValue && 'errTerms' in exprValue;
}

// export function evalAffineForm(aff: AffineForm, x: number[], y: number[]): { l: number[], u: number[] } {
// }
