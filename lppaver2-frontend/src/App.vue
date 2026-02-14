<script setup lang="ts">
import { computed, reactive } from 'vue';
import Paving2D from './paving2D/Paving2D.vue';
import { useStepsStore } from './steps/stepsStore';
import StepTree from './steps/StepTree.vue';
import { storeToRefs } from 'pinia';
import { GridLayout, GridItem, type LayoutItem } from 'grid-layout-plus'
import ProblemView from './ProblemView.vue';
import { type FormOrExprHash } from './formulas/forms';
import FormExprPlot from './formulas/FormExprPlot.vue';

const stepStore = useStepsStore()
const { focusedProblem, focusedProblemSubFormExpr, focusedExprValues } = storeToRefs(stepStore)

stepStore.initSession('default')

const focusedScopeH = computed(() => focusedProblem.value?.scope ?? null);
const focusedScopeBox = computed(() => !focusedScopeH.value ? null : stepStore.getBox(focusedScopeH.value));

const viewHeight = computed(() => window.innerHeight - 100);

const stepTreeLayout = reactive<LayoutItem>({ i: 'stepTree', x: 0, y: 0, w: 6, h: 3 });
const paving2DLayout = reactive<LayoutItem>({ i: 'paving2D', x: 0, y: 3, w: 6, h: 5 });
const focusedPLayout = reactive<LayoutItem>({ i: 'focusedP', x: 6, y: 0, w: 6, h: 3 });
const focEPlotLayout = reactive<LayoutItem>({ i: 'focEPlot', x: 6, y: 3, w: 6, h: 5 });

const layout = reactive<LayoutItem[]>([
  stepTreeLayout,
  paving2DLayout,
  focusedPLayout,
  focEPlotLayout,
]);

function onSubFormExprClicked(data: FormOrExprHash) {
  focusedProblemSubFormExpr.value = data;
}

</script>

<template>
  <GridLayout v-model:layout="layout" :colNum="12" :rowHeight="(viewHeight - 40) / 8" :isDraggable="true"
    :isResizable="true" :responsive="false" :useCssTransforms="true">
    <GridItem key="stepTree" i="stepTree" :x="stepTreeLayout.x" :y="stepTreeLayout.y" :w="stepTreeLayout.w"
      :h="stepTreeLayout.h" :isDraggable="false">
      <div class="border w-100 h-100" style="overflow-y: auto;">
        <StepTree />
      </div>
    </GridItem>
    <GridItem key="paving2D" i="paving2D" :x="paving2DLayout.x" :y="paving2DLayout.y" :w="paving2DLayout.w"
      :h="paving2DLayout.h" :isDraggable="false">
      <div class="border w-100 h-100" style="overflow-y: auto;">
        <Paving2D :topProblem="stepStore.zoomedProblem" />
      </div>
    </GridItem>
    <GridItem key="focusedP" i="focusedP" :x="focusedPLayout.x" :y="focusedPLayout.y" :w="focusedPLayout.w"
      :h="focusedPLayout.h" :isDraggable="false">
      <ProblemView :problem="focusedProblem" @click="onSubFormExprClicked"
        :highlightedSubFormExpr="focusedProblemSubFormExpr ?? undefined" />
    </GridItem>
    <GridItem key="focEPlot" i="focEPlot" :x="focEPlotLayout.x" :y="focEPlotLayout.y" :w="focEPlotLayout.w"
      :h="focEPlotLayout.h" :isDraggable="false">
      <div class="border w-100 h-100" style="overflow-y: auto;">
        <FormExprPlot v-if="focusedProblemSubFormExpr" :formOrExprHash="focusedProblemSubFormExpr"
          :box="focusedScopeBox ?? undefined" :exprValues="focusedExprValues" />
      </div>
    </GridItem>
  </GridLayout>
</template>
