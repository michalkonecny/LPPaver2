<script setup lang="ts">
import Plotly, { type ColorScale } from "plotly.js-dist-min";
import { computed, onMounted, ref, watch } from 'vue';
import { type FormOrExprHash } from './forms';
import { exprValueIsAffineForm, exprValueIsInterval, type Box, type ExprValue } from '@/steps/steps';
import { getExprVarExprs, type Var } from './exprs';
import { pickXY } from '@/boxes/pickVars';
import _ from 'lodash';
import { getTruthColour, useStepsStore } from '@/steps/stepsStore';
import { getCornersOnlyTriangulation, getHexTriangulation, type DomainAndN, type Triangulation2D } from './mesh';
import { evalFPExpr, evalFPForm } from "./evalFP";
import { kleeneanSwitch } from "./kleenean";

const props = defineProps<{
  formOrExprHash: FormOrExprHash | undefined;
  box: Box | undefined;
  exprValues?: Record<string, ExprValue>;
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

const exprValue = computed(() => {
  if (!expr.value) return undefined;
  if (!props.exprValues) return undefined;

  return props.exprValues[expr.value.hash];
});

const epxrValueBounds = computed(() => {
  const eValue = exprValue.value;
  if (!eValue) return undefined;
  const triang = cornersOnlyTriangulation.value;
  if (!triang) return undefined;

  if (exprValueIsInterval(eValue)) {
    const l: number[] = Array(triang.x.length).fill(eValue.l);
    const u: number[] = Array(triang.x.length).fill(eValue.u);
    return { l, u };
  }

  if (exprValueIsAffineForm(eValue)) {
    if (!props.exprValues) return undefined;
    // work out which var expr corresponds to x and y
    const varExprs = getExprVarExprs(expr.value!);
    const xExprHash = varExprs[xVar.value]?.hash;
    const yExprHash = varExprs[yVar.value]?.hash;
    // lookup the affine error term IDs corresponding to x and y
    const xExprValue = xExprHash ? props.exprValues[xExprHash] : undefined;
    const yExprValue = yExprHash ? props.exprValues[yExprHash] : undefined;
    // If expr value is an affine form, also the value for x and y will be affine forms.
    // The affine form for a variable will have a single error term, whose key is the variable's ID.
    const xErrId = xExprValue && exprValueIsAffineForm(xExprValue) ? _.keys(xExprValue.errTerms)[0] : undefined;
    const yErrId = yExprValue && exprValueIsAffineForm(yExprValue) ? _.keys(yExprValue.errTerms)[0] : undefined;

    const xErrCoeff: number = xErrId ? eValue.errTerms[xErrId] ?? 0 : 0;
    const yErrCoeff: number = yErrId ? eValue.errTerms[yErrId] ?? 0 : 0;
    const otherErrCoeffs: number[] = Object.entries(eValue.errTerms)
      .filter(([errId, _]) => errId !== xErrId && errId !== yErrId)
      .map(([_, coeff]) => coeff);
    const otherErrSum = _.sum(otherErrCoeffs.map(Math.abs));

    function xToUnit(x?: number): number {
      const xDom = xVarDomain.value;
      if (!xDom) return 0;
      const c = (xDom.l + xDom.u) / 2;
      return 2 * ((x ?? c) - c) / (xDom.u - xDom.l);
    }

    function yToUnit(y?: number): number {
      const yDom = yVarDomain.value;
      if (!yDom) return 0;
      const c = (yDom.l + yDom.u) / 2;
      return 2 * ((y ?? c) - c) / (yDom.u - yDom.l);
    }

    const c = triang.x.map((_, i) =>
      eValue.center
      + xErrCoeff * xToUnit(triang.x[i])
      + yErrCoeff * yToUnit(triang.y[i]));
    const l = c.map(v => v - otherErrSum);
    const u = c.map(v => v + otherErrSum);
    return { l, u };
  }

  return undefined;
});

function getBoundsTraces(): Trace[] {
  const triangulation = cornersOnlyTriangulation.value;
  const e = expr.value;
  const eValue = exprValue.value;
  if (!triangulation || !e || !eValue || !epxrValueBounds.value) { return []; }

  const colorscale: ColorScale = [[0, 'rgb(200,100,100)'], [1, 'rgb(200,100,100)']];
  const { l, u } = epxrValueBounds.value;
  function mkBoundTrace(z: Plotly.Datum[]): Trace {
    return {
      type: "mesh3d",
      z,
      colorscale,
      intensity: Array(z.length).fill(0),
      opacity: 0.5,
      showlegend: false,
      showscale: false,
      ...triangulation,
    };
  };
  const lBoundTrace: Trace = mkBoundTrace(l);
  const uBoundTrace: Trace = mkBoundTrace(u);
  return [lBoundTrace, uBoundTrace];
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