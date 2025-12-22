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

const topForm = computed(() => {
  const formHash = focusedProblem.value?.constraint;
  return formHash ? stepStore.getForm(formHash) : undefined;
})

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
          <FormattedForm :form="topForm" />
        </td>
      </tr>
    </tbody>
  </table>
</template>
