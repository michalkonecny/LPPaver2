<script lang="ts" setup>
import { storeToRefs } from 'pinia';
import { useStepsStore } from './stepsStore';
import StepTreeNode from './StepTreeNode.vue';

const stepStore = useStepsStore();

const { rootProblem } = storeToRefs(stepStore);

</script>

<template>
  <StepTreeNode v-if="rootProblem" :problem="rootProblem" />
  <div v-else class="text-center fst-italic mt-5">
    No steps available.
  </div>
</template>


<style lang="css" scoped-module>
.normal {
  border: 1px solid black;
}

.focused {
  border: 2px solid red;
}

.zoomed {
  border: 4px solid blue;
}

.focusedAndZoomed {
  border: 4px solid red;
}

table {
  width: 100%;
  /* margin-top: 10px; */
  /* margin-bottom: 10px; */
  /* border-collapse: collapse; */
}
</style>
