import { defineStore } from 'pinia';
import type { ExprValue } from '@/formulas/evalInfo';
import { problemToProblemHash, type Problem, type ProblemHash } from '@/problems/problems';
import { type ExprHash } from '../formulas/exprs';
import { type FormOrExprHash } from '../formulas/forms';
import { type Step } from './steps';

export const useStepsStore = defineStore('steps', {
  state: () => ({
    steps: [] as Step[],
    numberOfSteps: 0, // keep steps separately to make it easier to define reactive dependencies
    _problem2step: {} as Record<ProblemHash, Step>,
    rootProblem: null as Problem | null,
    focusedProblem: null as Problem | null,
    focusedProblemSubFormExpr: null as FormOrExprHash | null, // set in App.vue when user clicks on a sub-form or sub-expr
    zoomedProblem: null as Problem | null,
  }),
  actions: {
    async setProblem(problem: Problem) {
      const problemHash = problemToProblemHash(problem);
      const step: Step = { tag: 'GiveUpOnProblemStep', problem };
      this.steps.push(step);
      this._problem2step[problemHash] = step;
      this.rootProblem = problem;
      this.zoomedProblem = problem;
      this.focusedProblem = problem;
    },
    stepFromProblem(p: Problem) {
      const problemHash = problemToProblemHash(p);
      const step = this._problem2step[problemHash];
      if (!step) {
        throw new Error(`Step not found for problem hash ${problemHash}`);
      }
      return step;
    },
  },
  getters: {
    focusedExprValues(state): Record<ExprHash, ExprValue> | undefined {
      if (!state.focusedProblem) return undefined;
      const step = state._problem2step[problemToProblemHash(state.focusedProblem)];
      if (!step || step.tag !== 'ProgressStep') return undefined;
      return step.evalInfo.exprValues;
    },
  },
});
