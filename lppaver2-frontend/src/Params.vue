<script lang="ts" setup>
  import { computed, ref, watch } from 'vue';
  import { useStepsStore } from './steps/stepsStore';
  import { useProverStore } from './proverLink/proverStore.ts';

  const proverStore = useProverStore();
  const stepsStore = useStepsStore();

  const selectedProblemName = ref<string | null>(null);
  const selectedProblem = computed(() => {
    if (!selectedProblemName.value) {
      return null;
    }
    return proverStore.exampleProblems[selectedProblemName.value];
  });

  watch(selectedProblem, (newProblem) => {
    if (newProblem) {
      stepsStore.setProblem(newProblem.problem);
    }
  });
</script>

<template>
  <div>
    <!-- problem selector -->
    <div class="mb-2">
      <select class="form-select" v-model="selectedProblemName">
        <option :value="null">Select a problem</option>
        <option v-for="(p, name) in proverStore.exampleProblems" :key="name" :value="name">
          {{ name }}
        </option>
      </select>
    </div>
  </div>
</template>
