<script setup lang="ts">
import Plotly from "plotly.js-dist-min";
import { computed, onMounted, onUnmounted, ref, watch } from "vue";
import {
  decomposeBinaryComp,
  type Form,
  type FormOrExprHash,
} from "../formulas/forms";
import { type Box } from "@/steps/steps";
import { getExprVarExprs, type Expr, type Var } from "../formulas/exprs";
import { pickXY } from "@/boxes/pickVars";
import _ from "lodash";
import { getTruthColour, useStepsStore } from "@/steps/stepsStore";
import {
  getHexTriangulation,
  type DomainAndN,
  type Triangulation2D,
} from "../formulas/mesh";
import { evalFPExpr, evalFPForm } from "../formulas/evalFP";
import { kleeneanSwitch, type Kleenean } from "../formulas/kleenean";
import {
  evalExprOnTriangulation,
  intervalWidth,
  type ExprValue,
  type Interval,
} from "../formulas/evalInfo";
import { getKleeneanColourscale } from "./kleeneanColourscale";

const props = defineProps<{
  formOrExprHash: FormOrExprHash | undefined;
  box: Box | undefined;
  exprValues: Record<string, ExprValue>;
}>();

const plotDiv = ref<HTMLDivElement | null>(null);

const rootBox = computed(() => {
  const rootProblem = useStepsStore().rootProblem;
  if (!rootProblem) {
    return undefined;
  }
  return useStepsStore().getBox(rootProblem.scope);
});

const xVar = ref<Var>("_x");
const yVar = ref<Var>("_y");

watch(
  () => props.box,
  () => {
    if (!props.box) {
      return;
    }
    const xyVars = pickXY(props.box);
    xVar.value = xyVars.xVar;
    yVar.value = xyVars.yVar;
  },
  { immediate: true },
);

const yxRootRatio = computed<number>(() => {
  // extract root box domains for x and y vars
  const rootBoxDomains = rootBox.value?.box_.varDomains;
  if (!rootBoxDomains) {
    return 1;
  }
  const xDom = rootBoxDomains[xVar.value];
  const yDom = rootBoxDomains[yVar.value];
  if (!xDom || !yDom) {
    return 1;
  }

  return intervalWidth(yDom) / intervalWidth(xDom);
});

const yxRelativeRatio = computed<number>(() => {
  const boxDomains = props.box?.box_.varDomains;
  if (!boxDomains) {
    return 1;
  }
  const xDom = boxDomains[xVar.value];
  const yDom = boxDomains[yVar.value];
  if (!xDom || !yDom) {
    return 1;
  }

  const yxRatio = intervalWidth(yDom) / intervalWidth(xDom);

  return yxRatio / yxRootRatio.value;
});

const numberOfSamplesPerVar = ref(49);

const xVarDomain = computed(() => {
  if (!props.box) {
    return undefined;
  }
  return props.box.box_.varDomains[xVar.value];
});

const yVarDomain = computed(() => {
  if (!props.box) {
    return undefined;
  }
  return props.box.box_.varDomains[yVar.value];
});

const denseTriangulation = computed<Triangulation2D | undefined>(() => {
  if (!props.box) {
    return undefined;
  }

  if (!xVarDomain.value || !yVarDomain.value) {
    return undefined;
  }

  const xDomainAndN: DomainAndN = {
    domain: xVarDomain.value,
    n: numberOfSamplesPerVar.value,
  };
  const yDomainAndN: DomainAndN = {
    domain: yVarDomain.value,
    n: numberOfSamplesPerVar.value,
  };

  return getHexTriangulation(xDomainAndN, yDomainAndN);
});

/** eval environments for each sampled point in the triangulation */
const evalEnvs = computed(() => {
  const triangulation = denseTriangulation.value;
  if (!triangulation) {
    return [];
  }

  return _.zipWith(triangulation.x, triangulation.y, (x, y) => ({
    [xVar.value]: x,
    [yVar.value]: y,
  }));
});

const form = computed(() => {
  if (!props.formOrExprHash || props.formOrExprHash.type !== "form") {
    return undefined;
  }
  return useStepsStore().getForm(props.formOrExprHash.formHash);
});

const expr = computed(() => {
  if (!props.formOrExprHash || props.formOrExprHash.type !== "expr") {
    return undefined;
  }
  return useStepsStore().getExpr(props.formOrExprHash.exprHash);
});

const comparison = computed(() =>
  form.value ? decomposeBinaryComp(form.value) : {},
);

const zIsBool = computed(
  // z is boolean if we are plotting a form and it is not a comparison form
  // (because for comparison forms we plot the two numerical expressions)
  () => form.value !== undefined && !comparison.value.comp,
);

type Trace = Partial<Plotly.Data> & { intensity?: Plotly.Datum[] }; // intensity is missing in the Plotly type defs

function getFPValuesE(e: Expr) {
  return evalEnvs.value.map((env) => evalFPExpr(e, env));
}

function getFPValuesF(f: Form) {
  return evalEnvs.value.map((env) => evalFPForm(f, env));
}

const fpValues = computed(() => {
  if (form.value) {
    return getFPValuesF(form.value);
  } else if (expr.value) {
    return getFPValuesE(expr.value);
  } else {
    return undefined;
  }
});

const exprValue = computed<ExprValue | undefined>(() => {
  if (!expr.value) return undefined;

  return props.exprValues[expr.value.hash];
});

const exprValueBounds = computed<Interval<number[]> | undefined>(() => {
  // work out which var expr corresponds to x and y
  const varExprs = getExprVarExprs(expr.value!);
  const xExprHash = varExprs[xVar.value]?.hash;
  const yExprHash = varExprs[yVar.value]?.hash;
  // lookup the affine error term IDs corresponding to x and y
  const xExprValue = xExprHash ? props.exprValues[xExprHash] : undefined;
  const yExprValue = yExprHash ? props.exprValues[yExprHash] : undefined;

  return evalExprOnTriangulation(
    exprValue.value,
    xExprValue,
    yExprValue,
    xVarDomain.value,
    yVarDomain.value,
    denseTriangulation.value,
  );
});

const customdata = computed(() =>
  fpValues.value && zIsBool.value
    ? fpValues.value?.map((k) => [`${k === "CertainTrue"}`])
    : fpValues.value && expr.value && exprValueBounds.value
      ? (_.zip(
          fpValues.value as number[],
          exprValueBounds.value.l,
          exprValueBounds.value.u,
        ) as number[][])
      : undefined,
);

const hovertemplate = computed(
  () =>
    `${xVar.value} = %{x:.3f}<br>${yVar.value} = %{y:.3f}` +
    (!customdata.value
      ? ""
      : zIsBool.value
        ? "<br>val ⩬ %{customdata[0]}"
        : "<br>val ⩬ %{customdata[0]:.3f}<br>val ∈ [%{customdata[1]:.3f}, %{customdata[2]:.3f}]"),
);

const boolHeight = computed(() => 1);

function makeFPValueTrace(
  triangulation: Triangulation2D,
  z: number[],
  intensity: number[],
  colorscale: Plotly.ColorScale,
): Trace {
  return {
    type: "mesh3d",
    name: "",
    ...triangulation,
    z,
    intensity,
    colorscale,
    showlegend: false,
    showscale: false,
    customdata: customdata.value, // [fpValue, lowerBound, upperBound] for each point, if available
    hovertemplate: hovertemplate.value,
  };
}

const fpValueTraces = computed<Trace[]>(() => {
  const triangulation = denseTriangulation.value;
  const f = form.value;
  const e = expr.value;
  const { e1, e2, comp } = comparison.value;

  if ((!f && !e) || (e && f) || !triangulation) {
    return [];
  }

  let z: number[] = [];
  let colorscale: Plotly.ColorScale = [];

  if (f) {
    if (comp) {
      // a comparison form, show the values of both expressions
      // and use colour to indicate the result of the comparison

      const z1 = getFPValuesE(e1) as number[];
      const z2 = getFPValuesE(e2) as number[];

      /** z2 - z1 */
      const zdiff = z2.map((v, i) => v - z1[i]!);
      /** |z2 - z1| */
      const zdiffAbs = zdiff.map((v) => Math.abs(v));

      const intensity =
        comp === "CompLe" || comp === "CompLeq"
          ? zdiff // positive values indicate true and negative values indicate false
          : comp === "CompEq"
            ? zdiffAbs.map((v) => -v) // z1 == z2 is false except where z2-z1==0
            : zdiffAbs; // z1 != z2 is true where except where z2-z1=0

      const colorscale = getKleeneanColourscale(intensity, 0);

      const trace1 = makeFPValueTrace(triangulation, z1, intensity, colorscale);
      const trace2 = makeFPValueTrace(triangulation, z2, intensity, colorscale);

      return [trace1, trace2];
    }

    const kleeneans = fpValues.value as Kleenean[];
    z = kleeneans.map((k) =>
      kleeneanSwitch(k, boolHeight.value, 0, -boolHeight.value),
    );

    colorscale = getKleeneanColourscale(z, 0);
  }
  if (e) {
    z = fpValues.value as number[];
    colorscale = [
      [0, "rgb(0,0,200)"],
      [1, "rgb(0,200,200)"],
    ];
  }

  return [makeFPValueTrace(triangulation, z, z, colorscale)];
});

const boundsTraces = computed<Trace[]>(() => {
  const triangulation = denseTriangulation.value;
  const e = expr.value;
  const eValue = exprValue.value;
  if (!triangulation || !e || !eValue || !exprValueBounds.value) {
    return [];
  }

  const colorscale: Plotly.ColorScale = [
    [0, "rgb(200,100,100)"],
    [1, "rgb(200,100,100)"],
  ];
  const { l, u } = exprValueBounds.value;
  function mkBoundTrace(z: Plotly.Datum[]): Trace {
    return {
      type: "mesh3d",
      name: "",
      z,
      colorscale,
      intensity: Array(z.length).fill(0),
      opacity: 0.5,
      showlegend: false,
      showscale: false,
      ...triangulation,
      customdata: customdata.value, // [fpValue, lowerBound, upperBound] for each point, if available
      hovertemplate: hovertemplate.value,
      hoverinfo: "none", // do not show the trace name beside the template
    };
  }
  const lBoundTrace: Trace = mkBoundTrace(l);
  const uBoundTrace: Trace = mkBoundTrace(u);
  return [lBoundTrace, uBoundTrace];
});

const showExpValues = ref(true);
const showExpBounds = ref(true);

const showValues = computed({
  get() {
    return fpValueTraces.value.length > 0 ? showExpValues.value : false;
  },
  set(val: boolean) {
    if (fpValueTraces.value.length > 0) {
      showExpValues.value = val;
    }
  },
});
const showBounds = computed({
  get() {
    return boundsTraces.value.length > 0 ? showExpBounds.value : false;
  },
  set(val: boolean) {
    if (boundsTraces.value.length > 0) {
      showExpBounds.value = val;
    }
  },
});

const traces = computed<Trace[]>(() => {
  const result: Trace[] = [];
  if (showValues.value) {
    result.push(...fpValueTraces.value);
  }
  if (showBounds.value) {
    result.push(...boundsTraces.value);
  }
  return result;
});

function getAxisLayout(text: string): Partial<Plotly.LayoutAxis> {
  return {
    title: { text },
    // zeroline: false,
    showgrid: false,
  };
}

const aspectRatio = computed(() => {
  const ratio = yxRelativeRatio.value;
  if (ratio <= 1) {
    return { x: 1, y: ratio, z: 0.5 };
  } else {
    return { x: 1 / ratio, y: 1, z: 0.5 };
  }
});

const layout = computed<Partial<Plotly.Layout>>(() => ({
  autosize: true,
  uirevision: -1,
  scene: {
    aspectmode: "manual",
    aspectratio: aspectRatio.value,
    xaxis: { ...getAxisLayout(xVar.value) },
    yaxis: { ...getAxisLayout(yVar.value) },
    zaxis: {
      ...getAxisLayout("value"),
      tickvals: zIsBool.value
        ? [-boolHeight.value, 0, boolHeight.value]
        : undefined,
      ticktext: zIsBool.value ? ["false", "?", "true"] : undefined,
    },
  },
  margin: { t: 5, b: 5, l: 5, r: 5 },
}));

function renderPlot() {
  if (!plotDiv.value) {
    return;
  }

  Plotly.react(plotDiv.value, traces.value, layout.value, {
    displayModeBar: true,
    responsive: true,
    scrollZoom: true,
  });
}

watch([plotDiv, traces, layout], renderPlot);
onMounted(renderPlot);
onUnmounted(() => {
  if (plotDiv.value) {
    Plotly.purge(plotDiv.value);
  }
});
</script>

<template>
  <!-- TODO: make plot resize with grid cell resize -->
  <div ref="plotDiv" class="w-100" style="height: calc(100% - 35px)"></div>
  <div class="d-flex align-items-center gap-2 mt-2">
    <!-- values toggle -->
    <label class="form-check-label text-nowrap" for="valuesToggle"
      >Values:</label
    >
    <div class="form-check w-auto">
      <input
        :disabled="fpValueTraces.length === 0"
        class="form-check-input"
        type="checkbox"
        id="valuesToggle"
        v-model="showValues"
      />
    </div>

    <!-- bounds toggle -->
    <label class="form-check-label text-nowrap" for="boundsToggle"
      >Bounds:</label
    >
    <div class="form-check w-auto">
      <input
        :disabled="boundsTraces.length === 0"
        class="form-check-input"
        type="checkbox"
        id="boundsToggle"
        v-model="showBounds"
      />
    </div>

    <!-- triangulation resolution slider -->
    <label for="triangResSlider" class="text-nowrap">
      Triangulation Resolution:
    </label>
    <input
      id="triangResSlider"
      class="form-range form-control"
      type="range"
      min="11"
      max="199"
      step="2"
      v-model="numberOfSamplesPerVar"
    />
  </div>
</template>
