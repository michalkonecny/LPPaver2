<script setup lang="ts">
import { computed, reactive } from 'vue';
import FormattedForm from './formulas/FormattedForm.vue';
import Paving2D from './paving2D/Paving2D.vue';
import { useStepsStore } from './steps/stepsStore';
import StepTree from './steps/StepTree.vue';
import { storeToRefs } from 'pinia';
import { GridLayout, GridItem, type LayoutItem } from 'grid-layout-plus'

const stepStore = useStepsStore()
const { focusedProblem } = storeToRefs(stepStore)

stepStore.initSession('default')

const focusedForm = computed(() => {
  const formHash = focusedProblem.value?.constraint;
  return formHash ? stepStore.getForm(formHash) : undefined;
})

const focusedFormValues = computed(() => {
  // identify the focused progress step (if any)
  if (!focusedProblem.value) { return undefined; }
  const step = stepStore.stepFromProblem(focusedProblem.value);
  if (step.tag !== 'ProgressStep') { return undefined; }

  return step.evalInfo.formValues;
})

const focusedBox = computed(() => {
  const scope = focusedProblem.value?.scope;
  return scope ? stepStore.getBox(scope) : undefined;
});

const focusedBoxStr = computed(() => {
  if (!focusedBox.value) {
    return 'No box focused';
  }
  const varDomains = focusedBox.value.box_.varDomains;
  const parts = Object.entries(varDomains).map(([v, d]) => {
    return `${v}: [${d.l}, ${d.u}]`;
  });
  return parts.join(', ');
});

const viewHeight = computed(() => {
  return window.innerHeight - 100;
});

const stepTreeLayout = reactive<LayoutItem>({ i: 'stepTree', x: 0, y: 0, w: 6, h: 8 });
const paving2DLayout = reactive<LayoutItem>({ i: 'paving2D', x: 6, y: 0, w: 6, h: 5 });
const focusedPLayout = reactive<LayoutItem>({ i: 'focusedP', x: 6, y: 5, w: 6, h: 3 });

const layout = reactive<LayoutItem[]>([
  stepTreeLayout,
  paving2DLayout,
  focusedPLayout,
]);

</script>

<template>
  <GridLayout v-model:layout="layout" :colNum="12" :rowHeight="(viewHeight - 40) / 8" :isDraggable="true"
    :isResizable="true" :responsive="false" :useCssTransforms="true">
    <GridItem key="stepTree" i="stepTree" :x="stepTreeLayout.x" :y="stepTreeLayout.y" :w="stepTreeLayout.w"
      :h="stepTreeLayout.h">
      <div class="border w-100 h-100" style="overflow-y: auto;">
        <StepTree />
      </div>
    </GridItem>
    <GridItem key="paving2D" i="paving2D" :x="paving2DLayout.x" :y="paving2DLayout.y" :w="paving2DLayout.w"
      :h="paving2DLayout.h">
      <div class="border w-100 h-100" style="overflow-y: auto;">
        <Paving2D :topProblem="stepStore.zoomedProblem" />
      </div>
    </GridItem>
    <GridItem key="focusedP" i="focusedP" :x="focusedPLayout.x" :y="focusedPLayout.y" :w="focusedPLayout.w"
      :h="focusedPLayout.h">
      <div class="border w-100 h-100" style="overflow-y: auto;">
        <span class="d-flex justify-content-center mt-1">
          Focused problem:
        </span>
        <span class="d-flex justify-content-center mt-1">
          {{ focusedBoxStr }}
        </span>
        <span class="d-flex justify-content-center mt-1">
          <FormattedForm :form="focusedForm" :formValues="focusedFormValues" :widthLimit="60" />
        </span>
      </div>
    </GridItem>
  </GridLayout>
</template>
