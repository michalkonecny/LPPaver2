<script setup lang="ts">
import Paving2D from './paving2D/Paving2D.vue';
import { useStepsStore } from './steps/stepsStore';
import StepTree from './steps/StepTree.vue';

const stepStore = useStepsStore()
stepStore.initSession('default')

</script>

<template>
  <table>
    <tbody>
      <tr>
        <td style="width: 500px; height: 600px; padding-right: 20px;">
          <div style="max-width: 480px; max-height: 580px; overflow: auto;">
            <StepTree />
          </div>
        </td>
        <td style="width: calc(100% - 520px);">
          <Paving2D :topProblem="stepStore.zoomedProblem" />
        </td>
      </tr>
    </tbody>
  </table>
</template>
