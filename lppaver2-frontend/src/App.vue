<script setup lang="ts">
import { useStepsStore } from './steps/stepsStore';
import StepTree from './steps/StepTree.vue';

const stepStore = useStepsStore()
stepStore.initSession('default')

</script>

<template>
  <StepTree />
</template>


