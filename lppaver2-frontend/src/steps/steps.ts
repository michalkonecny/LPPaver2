import type { Var } from "./exprs"

export type ExprHash = string
export type FormHash = string
export type BoxHash = string

// Box types

export type Interval = {
  l: number,
  u: number
}

export type Box_ = {
  varDomains: Record<Var, Interval>,
  splitOrder: Var[]
}

export type Box = {
  boxHash: BoxHash,
  box_: Box_,
}

// Step types

export type Problem = {
  scope: BoxHash,
  constraint: FormHash
}

export type Paving = {
  scope: BoxHash,
  inner: BoxHash[],
  outer: BoxHash[],
  undecided: Problem[],
}

export type Step =
  | InitStep
  | PruneStep
  | SplitStep
  | GiveUpOnProblemStep
  | AbortStep
  | DoneStep

export type InitStep = {
  tag: 'InitStep',
  problem: Problem
}

export type PruneStep = {
  tag: 'PruneStep',
  problem: Problem,
  prunePaving: Paving
}

export type SplitStep = {
  tag: 'SplitStep',
  problem: Problem,
  pieces: Problem[]
}

export type GiveUpOnProblemStep = {
  tag: 'GiveUpOnProblemStep',
  problem: Problem
}

export type AbortStep = {
  tag: 'AbortStep',
  detail: string
}

export type DoneStep = {
  tag: 'DoneStep'
}
