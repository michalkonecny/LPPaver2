<script lang="ts" setup>
import { storeToRefs } from 'pinia';
import { computed } from 'vue';
import { useStepsStore } from './stepsStore';
import { getSubProblems, sameProblem, type Problem, type Step } from './steps';

const props = defineProps<{
  problem: Problem
}>();

const stepStore = useStepsStore();

const { focusedProblem, zoomedProblem } = storeToRefs(stepStore);

const step = stepStore.stepFromProblem(props.problem);

const subProblems = getSubProblems(step);

const isFocused = computed(() => sameProblem(props.problem, focusedProblem.value));
const isZoomed = computed(() => sameProblem(props.problem, zoomedProblem.value));

const classes = computed(() => {
  return {
    normal: !isFocused.value && !isZoomed.value,
    focused: isFocused.value && !isZoomed.value,
    zoomed: isZoomed.value && !isFocused.value,
    focusedAndZoomed: isFocused.value && isZoomed.value,
  };
});

function stepColour(step: Step) {
  const truthResult = stepStore.getStepTruthResult(step);

  switch (truthResult) {
    case "true":
      return "#e0ffe0";
    case "false":
      return "#ffd0e0";
    default:
      return "#e0e0ff";
  }
}

</script>

<template>
  <table :class="classes" :style="`background-color: ${stepColour(step)};`">
    <tr>
      <td colspan="2" class="text-left fw-bold">
        Step: {{ step.tag }}
      </td>
    </tr>
    <tr v-for="subProblem in subProblems">
      <td class="text-center" style="width: 15px; vertical-align: top; color: midnightblue;">↳</td>
      <td>
        <StepTreeNode :problem="subProblem" />
      </td>
    </tr>
  </table>
</template>
