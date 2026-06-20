<script lang="ts" setup>
  import { computed, ref, watch, type DeepReadonly } from 'vue';
  import { useStepsStore } from './steps/stepsStore';
  import { useProverStore, type ParamSpec } from './proverLink/proverStore.ts';

  const proverStore = useProverStore();
  const stepsStore = useStepsStore();

  const selectedProblemName = ref<string | null>(null);
  const selectedProblem = computed(() => {
    if (!selectedProblemName.value) {
      return null;
    }
    return proverStore.exampleProblems[selectedProblemName.value];
  });

  type ParamValue = {
    spec: DeepReadonly<ParamSpec>;
    val: number;
  };

  const params = ref<ParamValue[]>([]);

  function run() {
    if (!selectedProblemName.value) return;

    // transform params array into a record of paramName -> val
    const paramsObj: Record<string, number> = {};
    for (const param of params.value) {
      paramsObj[param.spec.paramName] = param.val;
    }

    proverStore.startRun(selectedProblemName.value, paramsObj);
  }

  watch(selectedProblem, (newProblem) => {
    if (newProblem) {
      stepsStore.setProblem(newProblem.problem);
      params.value = newProblem.paramSpecs.map((spec) => ({
        spec,
        val: spec.defaultValue,
      }));
    }
  });
</script>

<template>
  <div class="d-flex align-items-baseline">
    <!-- problem selector -->
    <div class="mb-2">
      <select class="form-select" v-model="selectedProblemName">
        <option :value="null">Select a problem</option>
        <option v-for="(p, name) in proverStore.exampleProblems" :key="name" :value="name">
          {{ name }}
        </option>
      </select>
    </div>
    <!-- parameter inputs -->
    <div v-if="params.length > 0">
      <div
        v-for="param in params"
        :key="param.spec.paramName"
        class="d-flex align-items-baseline mx-2"
      >
        <label :for="param.spec.paramName" class="form-label">{{ param.spec.paramName }}</label>
        <input
          type="number"
          class="form-control"
          :id="param.spec.paramName"
          v-model.number="param.val"
          :min="param.spec.minValue"
          :max="param.spec.maxValue"
          step="0.001"
        />
      </div>
    </div>
    <!-- run button -->
    <div class="mt-2">
      <button :disabled="!selectedProblemName" class="btn btn-primary" @click="run">Run</button>
    </div>
  </div>
</template>
