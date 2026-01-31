<script setup lang="ts">
import Plotly from "plotly.js-dist-min";
import { computed, onMounted, ref, watch } from 'vue';
import { type FormOrExprHash } from './forms';
import type { Box } from '@/steps/steps';
import type { Var } from './exprs';
import { pickXY } from '@/boxes/pickVars';
import _ from 'lodash';
import { useStepsStore } from '@/steps/stepsStore';
import { getHexTriangulation, type DomainAndN, type Triangulation2D } from './mesh';
import { evalFPExpr } from "./evalFP";

const props = defineProps<{
  formOrExprHash: FormOrExprHash | undefined;
  box: Box | undefined;
}>();

const plotDiv = ref<HTMLDivElement | null>(null);

// const topScopeVarDomains = computed(() => topScopeBox.value?.varDomains ?? {});
const vars = computed(() => props.box?.box_.splitOrder ?? []);

const xVar = ref<Var>("_x");
const yVar = ref<Var>("_y");

watch(() => props.box, () => {
  if (!props.box) { return; }
  const xyVars = pickXY(props.box);
  xVar.value = xyVars.xVar;
  yVar.value = xyVars.yVar;
}, { immediate: true });

const numberOfSamplesPerVar = 31;

const boxTriangulation = computed<Triangulation2D | undefined>(() => {
  if (!props.box) { return undefined; }

  const varDomains = props.box.box_.varDomains;
  const xVarDomain = varDomains[xVar.value];
  const yVarDomain = varDomains[yVar.value];
  if (!xVarDomain || !yVarDomain) { return undefined; }

  const xDomainAndN: DomainAndN = { domain: xVarDomain, n: numberOfSamplesPerVar };
  const yDomainAndN: DomainAndN = { domain: yVarDomain, n: numberOfSamplesPerVar };

  return getHexTriangulation(xDomainAndN, yDomainAndN);
});

const form = computed(() => {
  if (!props.formOrExprHash || props.formOrExprHash.type !== "form") { return undefined; }
  return useStepsStore().getForm(props.formOrExprHash.formHash);
});

const expr = computed(() => {
  if (!props.formOrExprHash || props.formOrExprHash.type !== "expr") { return undefined; }
  return useStepsStore().getExpr(props.formOrExprHash.exprHash);
});

const plotData = computed<Partial<Plotly.Data>[]>(() => {
  const triangulation = boxTriangulation.value;
  const f = form.value;
  const e = expr.value;
  if (!f && !e || !triangulation) { return []; }

  let z: Plotly.Datum[] = [];

  if (f) {
    z = triangulation.x.map(() => 0) // flat on z=0 plane for now
  } 
  if (e) {
    // eval environments for each sampled point
    const evalEnvs = _.zipWith(triangulation.x, triangulation.y, (x, y) =>
      ({ [xVar.value]: x, [yVar.value]: y }));

    z = evalEnvs.map(env => evalFPExpr(e, env));
  }
  const trace: Partial<Plotly.Data> = {
    type: "mesh3d",
    z,
    ...triangulation,
  };
  return [trace];
});

function getAxisLayout(text: string): Partial<Plotly.LayoutAxis> {
  return {
    title: { text },
    // zeroline: false,
    showgrid: false,
  };
};

const layout = computed<Partial<Plotly.Layout>>(() => ({
  autosize: true,
  uirevision: -1,
  scene: {
    aspectmode: "data",
    xaxis: { ...getAxisLayout(xVar.value) },
    yaxis: { ...getAxisLayout(yVar.value) },
    zaxis: { ...getAxisLayout("value") },
  },
  margin: { t: 5, b: 5, l: 40, r: 5 },
}));

let handlerAttached = false;

function renderPlot() {
  if (!plotDiv.value) { return; }

  Plotly.react(plotDiv.value, plotData.value, layout.value,
    {
      displayModeBar: true,
      responsive: true,
      scrollZoom: true,
    });

  // add a Plotly mouse click event handler
  if (plotDiv.value && !handlerAttached) {
    handlerAttached = true;
    // plotDiv.value.on('plotly_click', (data) => {
    //   // console.log('Plotly click event data:', data);
    //   const point = data.points[0];
    //   const scope = point?.data.customdata[0] as string | undefined;
    //   const constraint = point?.data.customdata[1] as string | undefined;
    //   if (scope && constraint) {
    //     focusedProblem.value = { scope, constraint };
    //   }
    // });
  }
}

onMounted(() => {
  watch([plotDiv, boxTriangulation, form, expr], renderPlot, { immediate: true });
});

</script>

<template>
  <!-- TODO: make plot resize with grid cell resize -->
  <div ref="plotDiv" class="w-100" style="height: 90%;"></div>
</template>