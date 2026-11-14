<script lang="ts" setup>
  import { computed } from 'vue';
  import { useStepsStore } from './steps/stepsStore';
  import type { FormOrExprHash } from './formulas/forms';
  import FormattedForm from './formulas/FormattedForm.vue';
  import BoxVarsShow from './boxes/BoxVarsShow.vue';
  import type { Problem } from './problems/problems';
  import { useProverStore } from './proverLink/proverStore.ts';

  const props = defineProps<{
    problem: Problem | null;
    highlightedSubFormExpr?: FormOrExprHash;
  }>();

  const emits = defineEmits<{
    (e: 'click', data: FormOrExprHash): void;
  }>();

  const stepsStore = useStepsStore();
  const proverStore = useProverStore();

  const box = computed(() => {
    const scope = props.problem?.scope;
    return scope ? proverStore.getBox(scope) : undefined;
  });

  const form = computed(() => {
    const formHash = props.problem?.constraint;
    return formHash ? proverStore.getForm(formHash) : undefined;
  });

  const formValues = computed(() => {
    // available only once the prover has sent the evaluation info for the problem's step
    if (!props.problem) {
      return undefined;
    }
    return stepsStore.evalInfoFromProblem(props.problem)?.formValues;
  });

  const viewWidth = computed(() => window.innerWidth);
</script>

<template>
  <div class="w-100 h-100" style="overflow-y: auto; border: solid 2px red">
    <BoxVarsShow v-if="box" :box="box" class="mb-2" />
    <span class="d-flex justify-content-center mt-1">
      <!-- TODO: adjust width dynamically with grid cell size changes -->
      <FormattedForm
        :form="form"
        :formValues="formValues"
        :inInfo="{
          widthLimit: Math.round(viewWidth / 20),
          couldBeFormType: 'IE',
        }"
        :highlightedSubFormExpr="highlightedSubFormExpr"
        @click="(d) => emits('click', d)"
      />
    </span>
  </div>
</template>
