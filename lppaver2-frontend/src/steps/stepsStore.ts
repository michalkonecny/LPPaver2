import { defineStore, storeToRefs } from 'pinia';
import { computed, ref, watch, type Ref } from 'vue';
import type { ExprValue } from '@/formulas/evalInfo';
import { problemToProblemHash, type Problem, type ProblemHash } from '@/problems/problems';
import { type ExprHash } from '../formulas/exprs';
import { type FormOrExprHash } from '../formulas/forms';
import { getStepProblem, type Step } from './steps';
import { useProverStore } from '@/proverLink/proverStore';

export const useStepsStore = defineStore('steps', () => {
  const steps: Ref<Step[]> = ref([]);
  const numberOfSteps = ref(0); // keep steps separately to make it easier to define reactive dependencies
  const _problem2step: Ref<Record<ProblemHash, Step>> = ref({});
  const rootProblem: Ref<Problem | null> = ref(null);
  const focusedProblem: Ref<Problem | null> = ref(null);
  const focusedProblemSubFormExpr: Ref<FormOrExprHash | null> = ref(null); // set in App.vue when user clicks on a sub-form or sub-expr
  const zoomedProblem: Ref<Problem | null> = ref(null);

  const focusedExprValues = computed<Record<ExprHash, ExprValue> | undefined>(() => {
    if (!focusedProblem.value) return undefined;
    const step = _problem2step.value[problemToProblemHash(focusedProblem.value)];
    if (!step || step.tag !== 'ProgressStep') return undefined;
    return step.evalInfo.exprValues;
  });

  const exports = {
    steps,
    numberOfSteps,
    _problem2step,
    rootProblem,
    focusedProblem,
    focusedProblemSubFormExpr,
    zoomedProblem,
    setProblem,
    stepFromProblem,
    focusedExprValues,
  };

  async function setProblem(problem: Problem) {
    const problemHash = problemToProblemHash(problem);
    const step: Step = { tag: 'GiveUpOnProblemStep', problem };
    steps.value.push(step);
    _problem2step.value[problemHash] = step;
    rootProblem.value = problem;
    zoomedProblem.value = problem;
    focusedProblem.value = problem;
  }

  function stepFromProblem(p: Problem) {
    const problemHash = problemToProblemHash(p);
    const step = _problem2step.value[problemHash];
    if (!step) {
      throw new Error(`Step not found for problem hash ${problemHash}`);
    }
    return step;
  }

  // track the steps of the current run
  const proverStore = useProverStore();
  const { runs, currentRunId } = storeToRefs(proverStore);
  const currentRunSteps = computed(() => {
    const steps = runs.value[currentRunId.value ?? '']?.steps;
    if (!steps || steps.length == 0) return undefined;
    return steps;
  });

  watch(currentRunSteps, () => {
    if (!currentRunSteps.value) return;
    steps.value = currentRunSteps.value;
    numberOfSteps.value = currentRunSteps.value.length;
    _problem2step.value = {};
    // for all steps...
    for (const step of currentRunSteps.value) {
      const problem = getStepProblem(step);
      if (problem) {
        // store the step for this problem
        const problemHash = problemToProblemHash(problem);
        _problem2step.value[problemHash] = step;
        // if this is the first step, set it as the root problem
        if (step.tag === 'InitStep') {
          setProblem(problem);
        }
      }
    }
  });

  return exports;
});
