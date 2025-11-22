<script lang="ts" setup>

import { computed, onMounted, ref, watch } from "vue";
import { storeToRefs } from "pinia";
import Plotly from "plotly.js-dist-min";

import { getSubProblems, type Problem } from "@/steps/steps";
import { useStepsStore } from "@/steps/stepsStore";
import type { Var } from "@/steps/exprs";

const props = withDefaults(defineProps<{
  topProblem: Problem | null;
  maxLevels?: number;
}>(), {
  maxLevels: 20
});

const stepsStore = useStepsStore();
const { focusedProblem } = storeToRefs(stepsStore);

const plotDiv = ref<HTMLDivElement | null>(null);

const topScopeH = computed(() => props.topProblem?.scope ?? null);
const topScopeBox = computed(() => !topScopeH.value ? null : stepsStore.getBox(topScopeH.value).box_);
// const topScopeVarDomains = computed(() => topScopeBox.value?.varDomains ?? {});
const topScopeVars = computed(() => topScopeBox.value?.splitOrder ?? []);

const xVar = ref<Var>(topScopeVars.value[0] || "x");
const yVar = ref<Var>(topScopeVars.value[1] || "y");

watch(topScopeVars, vars => {
  xVar.value = vars[0] ?? "_x"
  yVar.value = vars[1] ?? "_y"
})

function getProblemShapes(problem: Problem | null): Partial<Plotly.Shape>[] {
  if (!problem) { return []; }

  const step = stepsStore.stepFromProblem(problem);

  // recursively get subproblem shapes
  const subProblems = getSubProblems(step);
  const subProblemShapes: Partial<Plotly.Shape>[] = subProblems.flatMap(getProblemShapes);

  // get this problem's box shape
  const box = stepsStore.getBox(problem.scope);
  const varDomains = box.box_.varDomains
  const problemShape: Partial<Plotly.Shape> = {
    type: "rect",
    x0: varDomains[xVar.value]?.l ?? 0,
    x1: varDomains[xVar.value]?.u ?? 0,
    y0: varDomains[yVar.value]?.l ?? 0,
    y1: varDomains[yVar.value]?.u ?? 0,
    line: {
      color: "black",
      width: 1
    },
    fillcolor: stepsStore.getStepColour(step),
    opacity: 0.5,
  }

  return [problemShape, ...subProblemShapes];
}

function getFocusedProblemOutline(){
  if (!focusedProblem.value) { return []; }

  const box = stepsStore.getBox(focusedProblem.value.scope);
  const varDomains = box.box_.varDomains
  const outline: Partial<Plotly.Shape> = {
    type: "rect",
    x0: varDomains[xVar.value]?.l ?? 0,
    x1: varDomains[xVar.value]?.u ?? 0,
    y0: varDomains[yVar.value]?.l ?? 0,
    y1: varDomains[yVar.value]?.u ?? 0,
    line: {
      color: "red",
      width: 3
    },
    fillcolor: "rgba(0,0,0,0)", // transparent fill
  }

  return [outline];
}

function getAxisLayout(v: Var): Partial<Plotly.LayoutAxis> {
  return {
    // title: { text: v },
    zeroline: false,
    showgrid: false,
  };
};

const layout = computed(() => ({
  autosize: true,
  uirevision: -1,
  xaxis: getAxisLayout(xVar.value),
  yaxis: getAxisLayout(yVar.value),
  margin: { t: 20, b: 40, l: 40, r: 20 },
  shapes: [...getProblemShapes(props.topProblem), ...getFocusedProblemOutline()],
}));

function renderPlot() {
  if (!plotDiv.value) { return; }

  Plotly.react(plotDiv.value, [{ x: [], y: [] }], layout.value, { displayModeBar: false, responsive: true });
}

onMounted(() => {
  watch([plotDiv, () => props.topProblem, xVar, yVar, focusedProblem], renderPlot, { immediate: true });
});


</script>

<template>
  <table style="width: 500px; height: 500px;">
    <tbody>
      <tr>
        <td style="width: 50px; vertical-align: middle;">
          <select class="form-select" style="width: min-content;" v-model="yVar">
            <option v-for="v in topScopeVars" :key="v" :value="v">{{ v }}</option>
          </select>
        </td>
        <td>
          <div style="width: 450px; height: 450px;" ref="plotDiv"></div>
        </td>
      </tr>
      <tr>
        <td></td>
        <td>
          <div class="w100 d-flex justify-content-center">
            <select class="form-select" style="width: min-content;" v-model="xVar">
              <option v-for="v in topScopeVars" :key="v" :value="v">{{ v }}</option>
            </select>
          </div>
        </td>
      </tr>
    </tbody>
  </table>

</template>