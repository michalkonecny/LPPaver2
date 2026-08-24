import type { BoxHash } from '@/boxes/boxes';
import type { FormHash } from '@/formulas/forms';

export type Problem = {
  scope: BoxHash;
  constraint: FormHash;
};

export type ProblemHash = string;

export function problemToProblemHash(problem: {
  scope: BoxHash;
  constraint: FormHash;
}): ProblemHash {
  return `${problem.scope}|${problem.constraint}`;
}

export function sameProblem(p1: Problem | null, p2: Problem | null): boolean {
  if (!p1 || !p2) return false;
  return problemToProblemHash(p1) === problemToProblemHash(p2);
}
