<script setup lang="ts">
import Plotly from "plotly.js-dist-min";
import { computed, onMounted, ref, watch } from 'vue';
import { type FormOrExprHash } from './forms';
import type { Box } from '@/steps/steps';
import type { Var } from './exprs';
import { pickXY } from '@/boxes/pickVars';
import _ from 'lodash';
import { getTruthColour, useStepsStore } from '@/steps/stepsStore';
import { getHexTriangulation, type DomainAndN, type Triangulation2D } from './mesh';
import { evalFPExpr, evalFPForm } from "./evalFP";
import { kleeneanSwitch } from "./kleenean";

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

const numberOfSamplesPerVar = 99;

const xVarDomain = computed(() => {
  if (!props.box) { return undefined; }
  return props.box.box_.varDomains[xVar.value];
});

const yVarDomain = computed(() => {
  if (!props.box) { return undefined; }
  return props.box.box_.varDomains[yVar.value];
});

const domainWidth = computed(() => {
  if (!xVarDomain.value || !yVarDomain.value) { return 0; }
  const xWidth = (xVarDomain.value.u) - (xVarDomain.value.l);
  const yWidth = (yVarDomain.value.u) - (yVarDomain.value.l);
  return Math.max(xWidth, yWidth);
});

const boxTriangulation = computed<Triangulation2D | undefined>(() => {
  if (!props.box) { return undefined; }

  const varDomains = props.box.box_.varDomains;
  if (!xVarDomain.value || !yVarDomain.value) { return undefined; }

  const xDomainAndN: DomainAndN = { domain: xVarDomain.value, n: numberOfSamplesPerVar };
  const yDomainAndN: DomainAndN = { domain: yVarDomain.value, n: numberOfSamplesPerVar };

  return getHexTriangulation(xDomainAndN, yDomainAndN);
});

/** eval environments for each sampled point in the triangulation */
const evalEnvs = computed(() => {
  const triangulation = boxTriangulation.value;
  if (!triangulation) { return []; }

  return _.zipWith(triangulation.x, triangulation.y, (x, y) =>
    ({ [xVar.value]: x, [yVar.value]: y }));
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
  if (!f && !e || e && f || !triangulation) { return []; }

  let z: Plotly.Datum[] = [];
  let colorscale: Plotly.ColorScale = [];

  if (f) {
    const boolHeight = domainWidth.value / 10;
    z = evalEnvs.value.map(env =>
      kleeneanSwitch(evalFPForm(f, env), boolHeight, 0, -boolHeight));
    const zHasTrue = z.some(v => (v as number) > 0);
    const zHas0 = z.some(v => (v as number) === 0);
    const zHasFalse = z.some(v => (v as number) < 0);
    if (zHasTrue && zHasFalse) {
      colorscale = [[0, getTruthColour("CertainFalse")], [0.5, getTruthColour("TrueOrFalse")], [1, getTruthColour("CertainTrue")]];
    } else if (zHasTrue && zHas0) {
      colorscale = [[0, getTruthColour("TrueOrFalse")], [1, getTruthColour("CertainTrue")]];
    } else if (zHasFalse && zHas0) {
      colorscale = [[0, getTruthColour("CertainFalse")], [1, getTruthColour("TrueOrFalse")]];
    } else if (zHasTrue) {
      colorscale = [[0, getTruthColour("CertainTrue")], [1, getTruthColour("CertainTrue")]];
    } else if (zHasFalse) {
      colorscale = [[0, getTruthColour("CertainFalse")], [1, getTruthColour("CertainFalse")]];
    } else {
      colorscale = [[0, getTruthColour("TrueOrFalse")], [1, getTruthColour("TrueOrFalse")]];
    }
  }
  if (e) {
    z = evalEnvs.value.map(env => evalFPExpr(e, env));
    colorscale = [[0, 'rgb(0,0,200)'], [1, 'rgb(0,200,200)']];
  }
  const trace: Partial<Plotly.Data & { intensity: any }> = { // intensity is missing in the type defs
    type: "mesh3d",
    z,
    intensity: z,
    colorscale,
    showlegend: false,
    showscale: false,
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
  margin: { t: 5, b: 5, l: 5, r: 5 },
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