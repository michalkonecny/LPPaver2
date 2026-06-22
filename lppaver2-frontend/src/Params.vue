<script lang="ts" setup>
  import { computed, ref, watch, type DeepReadonly } from 'vue';
  import { storeToRefs } from 'pinia';
  import { useStepsStore } from './steps/stepsStore';
  import { useProverStore, type Arithmetic, type ParamSpec } from './proverLink/proverStore.ts';

  const stepsStore = useStepsStore();
  const proverStore = useProverStore();
  const { currentRunId, runs } = storeToRefs(proverStore);

  const currentRunInfo = computed(() => {
    if (!currentRunId.value) return null;
    return proverStore.getRunInfo(currentRunId.value);
  });

  const currentRunStatus = computed(() => currentRunInfo.value?.status ?? null);

  const canStartRun = computed(
    () => selectedProblemName.value !== null && currentRunStatus.value !== 'SolverRunning',
  );

  const selectedProblemName = ref<string | null>(null);
  const selectedProblem = computed(() => {
    if (!selectedProblemName.value) {
      return null;
    }
    return proverStore.exampleProblems[selectedProblemName.value];
  });

  const selectedArithmetic = ref<Arithmetic>('BallArithmetic');
  const giveUpAccuracy = ref<number>(0.001);

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

    proverStore.startRun(
      selectedProblemName.value,
      paramsObj,
      selectedArithmetic.value,
      giveUpAccuracy.value,
    );
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
    <div class="flex-grow-1">&nbsp;</div>
    <!-- Choice of arithmetic -->
    <select class="form-select w-auto" v-model="selectedArithmetic">
      <option value="BallArithmetic">MP Interval Arithmetic</option>
      <option value="AffineArithmetic">MP Affine Arithmetic</option>
    </select>
    <!-- Input max size of box before giving up -->
    <label for="giveUpAccuracy" class="form-label">Max box size: </label>
    <input
      type="number"
      class="form-control w-auto mx-2"
      id="giveUpAccuracy"
      v-model.number="giveUpAccuracy"
      step="0.01"
    />
    <!-- run button -->
    <div>
      <button :disabled="!canStartRun" class="btn btn-primary" @click="run">Run</button>
    </div>
  </div>
  <div class="d-flex align-items-baseline">
    <div class="mx-2">
      <span v-if="currentRunStatus">Current run status: {{ currentRunStatus }}</span>
      <span v-else>No run in progress</span>
    </div>
  </div>
</template>
