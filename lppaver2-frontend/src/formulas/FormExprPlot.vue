<script setup lang="ts">
import Plotly, { type ColorScale } from "plotly.js-dist-min";
import { computed, onMounted, ref, watch } from 'vue';
import { type FormOrExprHash } from './forms';
import { exprValueIsInterval, type Box, type ExprValue } from '@/steps/steps';
import type { Var } from './exprs';
import { pickXY } from '@/boxes/pickVars';
import _ from 'lodash';
import { getTruthColour, useStepsStore } from '@/steps/stepsStore';
import { getCornersOnlyTriangulation, getHexTriangulation, type DomainAndN, type Triangulation2D } from './mesh';
import { evalFPExpr, evalFPForm } from "./evalFP";
import { kleeneanSwitch } from "./kleenean";

const props = defineProps<{
  formOrExprHash: FormOrExprHash | undefined;
  box: Box | undefined;
  exprValue?: ExprValue;
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

const denseTriangulation = computed<Triangulation2D | undefined>(() => {
  if (!props.box) { return undefined; }

  const varDomains = props.box.box_.varDomains;
  if (!xVarDomain.value || !yVarDomain.value) { return undefined; }

  const xDomainAndN: DomainAndN = { domain: xVarDomain.value, n: numberOfSamplesPerVar };
  const yDomainAndN: DomainAndN = { domain: yVarDomain.value, n: numberOfSamplesPerVar };

  return getHexTriangulation(xDomainAndN, yDomainAndN);
});

const cornersOnlyTriangulation = computed<Triangulation2D | undefined>(() => {
  if (!props.box) { return undefined; }

  const varDomains = props.box.box_.varDomains;
  if (!xVarDomain.value || !yVarDomain.value) { return undefined; }

  return getCornersOnlyTriangulation(xVarDomain.value, yVarDomain.value);
});

/** eval environments for each sampled point in the triangulation */
const evalEnvs = computed(() => {
  const triangulation = denseTriangulation.value;
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

type Trace = Partial<Plotly.Data> & { intensity: any }; // intensity is missing in the Plotly type defs

function getFPValueTrace(): Trace[] {
  const triangulation = denseTriangulation.value;
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

  return [{
    type: "mesh3d",
    z,
    intensity: z,
    colorscale,
    showlegend: false,
    showscale: false,
    ...triangulation,
  }];
}

function getBoundsTraces(): Trace[] {
  const triangulation = cornersOnlyTriangulation.value;
  const e = expr.value;
  const eValue = props.exprValue;
  if (!triangulation || !e || !eValue) { return []; }

  if (exprValueIsInterval(eValue)) {
    const { l, u } = eValue;
    const colorscale: ColorScale = [[0, 'rgb(200,100,100)'], [1, 'rgb(200,100,100)']];
    const lBoundTrace: Trace = {
      type: "mesh3d",
      z: Array(triangulation.x.length).fill(l),
      colorscale,
      intensity: Array(triangulation.x.length).fill(l),
      opacity: 0.5,
      showlegend: false,
      showscale: false,
      ...triangulation,
    };
    const uBoundTrace: Trace = {
      type: "mesh3d",
      z: Array(triangulation.x.length).fill(u),
      colorscale,
      intensity: Array(triangulation.x.length).fill(u),
      opacity: 0.5,
      showlegend: false,
      showscale: false,
      ...triangulation,
    };
    return [lBoundTrace, uBoundTrace];
  }

  return [];
}

const plotData = computed<Trace[]>(() => {
  const fpValueTraces = getFPValueTrace();

  const boundsTraces: Trace[] = getBoundsTraces();

  return [...fpValueTraces, ...boundsTraces];
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

function renderPlot() {
  if (!plotDiv.value) { return; }

  Plotly.react(plotDiv.value, plotData.value, layout.value,
    {
      displayModeBar: true,
      responsive: true,
      scrollZoom: true,
    });
}

onMounted(() => {
  watch([plotDiv, denseTriangulation, form, expr], renderPlot, { immediate: true });
});

</script>

<template>
  <!-- TODO: make plot resize with grid cell resize -->
  <div ref="plotDiv" class="w-100" style="height: 90%;"></div>
</template>