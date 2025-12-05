<script lang="ts" setup>
import { storeToRefs } from 'pinia';
import { computed, getCurrentInstance, ref, watch } from 'vue';
import { useStepsStore } from './stepsStore';
import { getSubProblems, sameProblem, type Problem, type Step } from './steps';

const props = defineProps<{
  problem: Problem
}>();

const stepsStore = useStepsStore();

const { focusedProblem, zoomedProblem } = storeToRefs(stepsStore);

const step = stepsStore.stepFromProblem(props.problem);

const stepTruth = stepsStore.getStepTruthResult(step);

const stepTruthNote = computed(() => {
  switch (stepTruth) {
    case "true":
      return "(True)";
    case "false":
      return "(False)";
    default:
      return "";
  }
});

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
    // Scroll this element into view if not visible
    const elementTop = el.value?.getBoundingClientRect().top ?? 0;
    if (elementTop < 0) { // top is above viewport
      el.value?.scrollIntoView({ behavior: 'smooth', block: 'start', inline: 'center' });
    }
  }
});

</script>

<template>
  <table ref="el" :class="classes" :style="`background-color: ${stepsStore.getStepColour(step)};`" @click="focusHere"
    @dblclick="zoomHere">
    <tr>
      <td colspan="2" class="text-left fw-bold">
        {{ step.tag }} {{ stepTruthNote }}
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
