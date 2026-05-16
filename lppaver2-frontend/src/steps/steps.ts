import type { Boxes, BoxHash } from '@/boxes/boxes';
import type { EvalInfo } from '@/formulas/evalInfo';
import type { Problem } from '@/problems/problems';

export type Step = InitStep | ProgressStep | GiveUpOnProblemStep | AbortStep | DoneStep;

export type InitStep = {
  tag: 'InitStep';
  problem: Problem;
};

export type ProgressStep = {
  tag: 'ProgressStep';
  problem: Problem;
  progressPaving: Paving;
  evalInfo: EvalInfo;
};

export type Paving = {
  scope: BoxHash;
  inner: Boxes;
  outer: Boxes;
  undecided: Problem[];
};

export function isSplitStep(step: ProgressStep): boolean {
  return step.progressPaving.undecided.length > 1;
}

export function isPruneStep(step: ProgressStep): boolean {
  return step.progressPaving.inner.boxes.length > 0 || step.progressPaving.outer.boxes.length > 0;
}

export type GiveUpOnProblemStep = {
  tag: 'GiveUpOnProblemStep';
  problem: Problem;
};

export type AbortStep = {
  tag: 'AbortStep';
  detail: string;
};

export type DoneStep = {
  tag: 'DoneStep';
};

export function getStepProblem(step: Step): Problem | null {
  return 'problem' in step ? step.problem : null;
}

export function getSubProblems(step: Step): Problem[] {
  switch (step.tag) {
    case 'ProgressStep':
      return [...step.progressPaving.undecided];
    default:
      return [];
  }
}
