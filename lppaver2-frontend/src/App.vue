<script setup lang="ts">
import { computed } from 'vue';
import FormattedForm from './formulas/FormattedForm.vue';
import Paving2D from './paving2D/Paving2D.vue';
import { useStepsStore } from './steps/stepsStore';
import StepTree from './steps/StepTree.vue';
import { storeToRefs } from 'pinia';

const stepStore = useStepsStore()
const { focusedProblem } = storeToRefs(stepStore)

stepStore.initSession('default')

const focusedForm = computed(() => {
  const formHash = focusedProblem.value?.constraint;
  return formHash ? stepStore.getForm(formHash) : undefined;
})

const focusedFormValues = computed(() => {
  // identify the focused progress step (if any)
  if (!focusedProblem.value) { return undefined; }
  const step = stepStore.stepFromProblem(focusedProblem.value);
  if (step.tag !== 'ProgressStep') { return undefined; }

  return step.evalInfo.formValues;
})

const focusedBox = computed(() => {
  const scope = focusedProblem.value?.scope;
  return scope ? stepStore.getBox(scope) : undefined;
});

const focusedBoxStr = computed(() => {
  if (!focusedBox.value) {
    return 'No box focused';
  }
  const varDomains = focusedBox.value.box_.varDomains;
  const parts = Object.entries(varDomains).map(([v, d]) => {
    return `${v}: [${d.l}, ${d.u}]`;
  });
  return parts.join(', ');
});

</script>

<template>
  <table>
    <tbody>
      <tr>
        <td style="width: 500px; height: 600px; padding-right: 20px;" rowspan="2">
          <div style="max-width: 480px; max-height: 580px; overflow: auto;">
            <StepTree />
          </div>
        </td>
        <td style="width: calc(100% - 520px);">
          <Paving2D :topProblem="stepStore.zoomedProblem" />
        </td>
      </tr>
      <tr>
        <td style="width: calc(100% - 520px);">
          <span class="d-flex justify-content-center mt-1">
            Focused problem:
          </span>
          <span class="d-flex justify-content-center mt-1">
            {{ focusedBoxStr }}
          </span>
          <span class="d-flex justify-content-center mt-1">
            <FormattedForm :form="focusedForm" :formValues="focusedFormValues" :widthLimit="60" />
          </span>
        </td>
      </tr>
    </tbody>
  </table>
</template>
