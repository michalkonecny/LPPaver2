<script lang="ts" setup>
import { storeToRefs } from 'pinia';
import { computed, ref, watch } from 'vue';
import { useStepsStore } from './stepsStore';
import { getSubProblems, sameProblem, type Problem } from './steps';

const props = defineProps<{
  problem: Problem
}>();

const stepsStore = useStepsStore();

const { focusedProblem, zoomedProblem } = storeToRefs(stepsStore);

const step = stepsStore.stepFromProblem(props.problem);

const stepTruth = stepsStore.getStepTruthResult(step);

const stepTruthNote =
  stepTruth === "true" ? " (True)" :
    stepTruth === "false" ? " (False)" :
      "";

const stepCategory = step.tag === "ProgressStep" ? 
  (step.progressPaving.undecided.length > 1 ? "Split" : "Prune") : step.tag;

const stepLabel = `${stepCategory}${stepTruthNote}`;

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

// Focus this problem when clicked
function focusHere(event: MouseEvent) {
  focusedProblem.value = props.problem;
  // prevent event bubbling
  event.stopPropagation();
}

// Zoom this problem when double-clicked
function zoomHere(event: MouseEvent) {
  zoomedProblem.value = props.problem;
  // prevent event bubbling
  event.stopPropagation();
}

const el = ref<HTMLElement | null>(null);

// Scroll to here when focused
watch(focusedProblem, (newVal) => {
  if (sameProblem(newVal, props.problem)) {
    // Scroll this element into view
    el.value?.scrollIntoView({ behavior: 'smooth', block: 'start', inline: 'center' });
  }
});

</script>

<template>
  <table ref="el" :class="classes" :style="`background-color: ${stepsStore.getStepColour(step)};`" @click="focusHere"
    @dblclick="zoomHere">
    <tbody>
      <tr>
        <td colspan="2" class="text-left fw-bold">
          {{ stepLabel }}
        </td>
      </tr>
      <tr v-for="subProblem in subProblems">
        <td class="text-center" style="width: 15px; vertical-align: top; color: midnightblue;">↳</td>
        <td>
          <StepTreeNode :problem="subProblem" />
        </td>
      </tr>
    </tbody>
  </table>
</template>
