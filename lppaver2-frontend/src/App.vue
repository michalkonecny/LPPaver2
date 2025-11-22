<script setup lang="ts">
import Paving2D from './paving2D/Paving2D.vue';
import { useStepsStore } from './steps/stepsStore';
import StepTree from './steps/StepTree.vue';

const stepStore = useStepsStore()
stepStore.initSession('default')

</script>

<template>
  <div class="row">
    <div class="col-6">
      <StepTree />
    </div>
    <div class="col-6">
      <Paving2D :topProblem="stepStore.rootProblem" />
    </div>
  </div>
</template>
