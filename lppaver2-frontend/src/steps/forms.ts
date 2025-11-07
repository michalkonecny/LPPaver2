import type { Expr } from "./exprs"

export type Form = { f: FormF<Expr, Form> }

export type FormF<E,F> = FormComp<E> | FormUnary<F> | FormBinary<F>
  | FormIfThenElse<F> | FormTrue | FormFalse

export type FormComp<E> = {
  tag: 'FormComp',
  comp: BinaryComp,
  e1: E,
  e2: E
}

export type FormUnary<F> = {
  tag: 'FormUnary',
  uconn: UnaryConn,
  f1: F
}

export type FormBinary<F> = {
  tag: 'FormBinary',
  bconn: BinaryConn,
  f1: F,
  f2: F,
}

export type FormIfThenElse<F> = {
  tag: 'FormIfThenElse',
  fc: F,
  ft: F,
  ff: F
}

export type FormTrue = {
  tag: 'FormTrue'
}

export type FormFalse = {
  tag: 'FormFalse'
}

export type BinaryComp = 'CompLe' | 'CompLeq' | 'CompEq' | 'CompNeq'
export type UnaryConn = 'ConnNeg'
export type BinaryConn = 'ConnAnd' | 'ConnOr' | 'ConnImpl'


