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

const plotDiv = ref<Plotly.PlotlyHTMLElement | null>(null);

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

function getProblemTraces(problem: Problem | null): Partial<Plotly.Data>[] {
  if (!problem) { return []; }

  const step = stepsStore.stepFromProblem(problem);

  // recursively get subproblem shapes
  const subProblems = getSubProblems(step);
  const subProblemTraces: Partial<Plotly.Data>[] = subProblems.flatMap(getProblemTraces);

  // get this problem's box shape
  const box = stepsStore.getBox(problem.scope);
  const constraint = stepsStore.getForm(problem.constraint);
  const varDomains = box.box_.varDomains;
  const x0 = varDomains[xVar.value]?.l ?? 0;
  const x1 = varDomains[xVar.value]?.u ?? 0;
  const y0 = varDomains[yVar.value]?.l ?? 0;
  const y1 = varDomains[yVar.value]?.u ?? 0;

  const boxDescr = `${xVar.value}:[${x0}, ${x1}]<br>${yVar.value}:[${y0}, ${y1}]`;

  const problemLineTrace: Partial<Plotly.Data> = {
    type: "scatter",
    x: [x0, x1, x1, x0, x0],
    y: [y0, y0, y1, y1, y0],
    mode: "lines",
    fill: "toself",
    line: {
      color: "black",
      width: 1
    },
    fillcolor: stepsStore.getStepColour(step),
    opacity: 0.5,
    showlegend: false,
    customdata: [problem.scope, problem.constraint],
    text: boxDescr,
    hoverinfo: "text",
  }

  return [problemLineTrace, ...subProblemTraces];
}

function getFocusedProblemOutline() {
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
    showlegend: false,
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

const layout = computed<Partial<Plotly.Layout>>(() => ({
  autosize: true,
  uirevision: -1,
  xaxis: getAxisLayout(xVar.value),
  yaxis: getAxisLayout(yVar.value),
  margin: { t: 20, b: 40, l: 40, r: 20 },
  shapes: [...getFocusedProblemOutline()],
  dragmode: "pan",
}));

let handlerAttached = false;

function renderPlot() {
  if (!plotDiv.value) { return; }

  Plotly.react(plotDiv.value, getProblemTraces(props.topProblem), layout.value,
    {
      displayModeBar: true,
      responsive: true,
      scrollZoom: true,
    });

  // add a Plotly mouse click event handler
  if (plotDiv.value && !handlerAttached) {
    handlerAttached = true;
    plotDiv.value.on('plotly_click', (data) => {
      console.log('Plotly click event data:', data);
      const point = data.points[0];
      const scope = point?.data.customdata[0] as number | undefined;
      const constraint = point?.data.customdata[1] as number | undefined;
      if (scope && constraint) {
        focusedProblem.value = { scope, constraint };
      }
    });
  }
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