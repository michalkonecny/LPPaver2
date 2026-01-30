<script lang="ts" setup>
import { computed } from 'vue';
import type { Problem } from './steps/steps';
import { useStepsStore } from './steps/stepsStore';
import type { FormOrExprHash } from './formulas/forms';
import FormattedForm from './formulas/FormattedForm.vue';

const props = defineProps<{
  problem: Problem | null;
  highlightedSubFormExpr?: FormOrExprHash;
}>();

const emits = defineEmits<{
  (e: 'click', data: FormOrExprHash): void;
}>();

const stepsStore = useStepsStore();

const box = computed(() => {
  const scope = props.problem?.scope;
  return scope ? stepsStore.getBox(scope) : undefined;
});

const variableRangeStrs = computed(() => {
  if (!props.problem) {
    return 'No box focused';
  }
  const varDomains = box.value?.box_.varDomains ?? [];
  const parts = Object.entries(varDomains).map(([v, d]) => {
    return `${v}: [${d.l}, ${d.u}]`;
  });
  return parts;
});

const form = computed(() => {
  const formHash = props.problem?.constraint;
  return formHash ? stepsStore.getForm(formHash) : undefined;
})

const formValues = computed(() => {
  // identify the focused progress step (if any)
  if (!props.problem) { return undefined; }
  const step = stepsStore.stepFromProblem(props.problem);
  if (step.tag !== 'ProgressStep') { return undefined; }

  return step.evalInfo.formValues;
})

const viewWidth = computed(() => window.innerWidth);

</script>

<template>
  <div class="w-100 h-100" style="overflow-y: auto; border: solid 2px red;">
    <span class="d-flex flex-column align-items-center mt-1">
      <span v-for="varRange in variableRangeStrs">
        {{ varRange }}
      </span>
    </span>
    <span class="d-flex justify-content-center mt-1">
      <!-- TODO: adjust width dynamically with grid cell size changes -->
      <FormattedForm :form="form" :formValues="formValues" :widthLimit="Math.round(viewWidth / 20)"
        :highlightedSubFormExpr="highlightedSubFormExpr" @click="(d) => emits('click', d)" />
    </span>
  </div>
</template>