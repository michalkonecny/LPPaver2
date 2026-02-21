import type { Var } from "../formulas/exprs"
import type { FormHash } from "../formulas/forms"
import type { EvalInfo, Interval } from "@/formulas/evalInfo"

export type BoxHash = string

// Box types

export type Box_ = {
  varDomains: Record<Var, Interval<number>>,
  splitOrder: Var[]
}

export type Box = {
  boxHash: BoxHash,
  box_: Box_,
}

export type Boxes = {
  boxes: BoxHash[]
}

// Step types

export type Problem = {
  scope: BoxHash,
  constraint: FormHash
}

export type ProblemHash = string

export function problemToProblemHash(problem: { scope: BoxHash, constraint: FormHash }): ProblemHash {
  return `${problem.scope}|${problem.constraint}`
}

export function sameProblem(p1: Problem | null, p2: Problem | null): boolean {
  if (!p1 || !p2) return false;
  return problemToProblemHash(p1) === problemToProblemHash(p2);
}

export type Paving = {
  scope: BoxHash,
  inner: Boxes,
  outer: Boxes,
  undecided: Problem[],
}

export type Step =
  | InitStep
  | ProgressStep
  | GiveUpOnProblemStep
  | AbortStep
  | DoneStep

export type InitStep = {
  tag: 'InitStep',
  problem: Problem
}

export type ProgressStep = {
  tag: 'ProgressStep',
  problem: Problem,
  progressPaving: Paving,
  evalInfo: EvalInfo,
}

export function isSplitStep(step: ProgressStep): boolean {
  return step.progressPaving.undecided.length > 1;
}

export function isPruneStep(step: ProgressStep): boolean {
  return step.progressPaving.inner.boxes.length > 0 || step.progressPaving.outer.boxes.length > 0;
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

export function getStepProblem(step: Step): Problem | null {
  return "problem" in step ? step.problem : null;
}

export function getSubProblems(step: Step): Problem[] {
  switch (step.tag) {
    case "ProgressStep":
      return [...step.progressPaving.undecided];
    default:
      return [];
  }
}
