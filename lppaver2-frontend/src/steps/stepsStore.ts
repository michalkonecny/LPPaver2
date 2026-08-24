import { defineStore, storeToRefs } from 'pinia';
import { computed, ref, watch, type Ref } from 'vue';
import type { ExprValue } from '@/formulas/evalInfo';
import { problemToProblemHash, type Problem, type ProblemHash } from '@/problems/problems';
import { type ExprHash } from '../formulas/exprs';
import { type FormOrExprHash } from '../formulas/forms';
import { getStepProblem, type Step } from './steps';
import { useProverStore } from '@/proverLink/proverStore';
import { getBoxVolume } from '@/boxes/boxes';

export type ProverStateStats = {
  percentInner: number;
  percentOuter: number;
  percentUnknown: number;
};

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

  const stepsStats = computed<ProverStateStats | undefined>(() => {
    if (!rootProblem.value) return undefined;

    // compute the total volume of the root problem's box
    const proverStore = useProverStore();
    const rootBox = proverStore.getBox(rootProblem.value.scope);
    const totalVolume = getBoxVolume(rootBox);

    // compute the volumes of the boxes in the steps
    let volumeInner = 0;
    let volumeOuter = 0;
    let volumeUnknown = 0;
    steps.value.forEach((step) => {
      if (step.tag === 'ProgressStep') {
        // progress steps
        step.progressPaving.inner.boxes.forEach((boxH) => {
          const box = proverStore.getBox(boxH);
          volumeInner += getBoxVolume(box);
        });
        step.progressPaving.outer.boxes.forEach((boxH) => {
          const box = proverStore.getBox(boxH);
          volumeOuter += getBoxVolume(box);
        });
      } else if (step.tag === 'GiveUpOnProblemStep') {
        volumeUnknown += getBoxVolume(proverStore.getBox(step.problem.scope));
      }
    });

    return {
      percentInner: (100 * volumeInner) / totalVolume,
      percentOuter: (100 * volumeOuter) / totalVolume,
      percentUnknown: (100 * volumeUnknown) / totalVolume,
    };
  });

  const exports = {
    steps,
    numberOfSteps,
    _problem2step,
    rootProblem,
    focusedProblem,
    focusedProblemSubFormExpr,
    zoomedProblem,
    previewProblem,
    setInitProblem,
    stepFromProblem,
    focusedExprValues,
    stepsStats,
  };

  async function previewProblem(problem: Problem) {
    const step: Step = { tag: 'GiveUpOnProblemStep', problem };
    steps.value.push(step);
    const problemHash = problemToProblemHash(problem);
    _problem2step.value[problemHash] = step;
    setInitProblem(problem);
  }

  async function setInitProblem(initProblem: Problem) {
    rootProblem.value = initProblem;
    zoomedProblem.value = initProblem;
    focusedProblem.value = initProblem;
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
    const stepsExceptInit = currentRunSteps.value.filter((step) => step.tag !== 'InitStep');
    steps.value = stepsExceptInit;
    numberOfSteps.value = stepsExceptInit.length;
    _problem2step.value = {};
    // build the problem2step mapping for all steps except InitStep
    for (const step of stepsExceptInit) {
      const problem = getStepProblem(step);
      if (problem) {
        const problemHash = problemToProblemHash(problem);
        _problem2step.value[problemHash] = step;
      }
    }
    // set the root problem to the problem of the first step (if it exists)
    if (stepsExceptInit.length > 0) {
      const firstStepProblem = getStepProblem(stepsExceptInit[0]!);
      if (firstStepProblem) {
        setInitProblem(firstStepProblem);
      }
    }
  });

  return exports;
});
