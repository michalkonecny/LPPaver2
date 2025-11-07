<script setup lang="ts">
import { useStepsStore } from './steps/stepsStore';

const stepStore = useStepsStore()
stepStore.initSession('default')

</script>

<template>
  <div>
    You did it!
  </div>
</template>


