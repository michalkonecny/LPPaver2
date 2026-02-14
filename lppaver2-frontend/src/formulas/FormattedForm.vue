<script lang="ts" setup>
import { binaryCompSymbolMap, binaryConnSymbolMap, unaryConnSymbolMap, type Form, type FormOrExprHash } from './forms';
import { computed, ref, watch, type StyleValue } from 'vue';
import type { FormValues } from '@/steps/steps';
import FormattedExpr from './FormattedExpr.vue';
import { getTruthColour } from '@/steps/stepsStore';
import _ from 'lodash';

const props = defineProps<{
  form?: Form;
  formValues?: FormValues;
  widthLimit: number;
  highlightedSubFormExpr?: FormOrExprHash;
}>();

const emits = defineEmits<{
  (e: 'width', width: number): void;
  (e: 'click', data: FormOrExprHash): void;
}>();

function emitWidth(width: number) {
  emits('width', width);
}

function emitWidthFromString(result: string) {
  emitWidth(result.length);
  return result;
}

const truthValue = computed(() => {
  if (!props.form || !props.formValues) {
    return null;
  }
  const h = props.form.hash;
  return props.formValues[h];
})

const colour = computed(() => getTruthColour(truthValue.value ?? 'TrueOrFalse'));

////////////////////////////
// for binary comparisons
////////////////////////////

const binaryCompSymbol = computed(() => {
  return props.form && props.form.f.tag === 'FormComp'
    ? binaryCompSymbolMap[props.form.f.comp]
    : '';
});

const binaryCompChildWidthLimit = computed(() => {
  // assuming horizontal layout (worst case)
  return props.widthLimit - 1; // 1 for space
});

const e1Width = ref<number | null>(null);
const e2Width = ref<number | null>(null);

function setE1Width(w: number) { e1Width.value = w; }
function setE2Width(w: number) { e2Width.value = w; }

const binaryCompTotalWidthIfHorizontal = computed(() => {
  if (e1Width.value === null || e2Width.value === null) {
    return 0;
  }
  return e1Width.value + e2Width.value + binaryCompSymbol.value.length + 2; // 2 for spaces
});

const binaryCompTotalWidthIfVertical = computed(() => {
  if (e1Width.value === null || e2Width.value === null) {
    return 0;
  }
  return Math.max(e1Width.value!, e2Width.value!) + 1; // 1 for space
});

const binaryCompFitsHorizontal = computed(() => {
  return binaryCompTotalWidthIfHorizontal.value <= props.widthLimit;
});

// when both e1Width and e2Width are set, emit total width
watch(binaryCompTotalWidthIfHorizontal, w => {
  // binary comparison, both children's widths are needed
  if (props.form?.f.tag === 'FormComp') {
    if (e1Width.value !== null && e2Width.value !== null) {
      if (binaryCompFitsHorizontal.value) {
        // enough horizontal space, using horizontal layout
        emitWidth(binaryCompTotalWidthIfHorizontal.value);
      } else {
        // not enough horizontal space, using vertical layout
        emitWidth(binaryCompTotalWidthIfVertical.value);
      }
    }
  }
});

////////////////////////////
// for unary logical connectors
////////////////////////////

const unaryConnSymbol = computed(() => {
  return props.form && props.form.f.tag === 'FormUnary'
    ? unaryConnSymbolMap[props.form.f.uconn]
    : '';
});

const unaryChildWidthLimit = computed(() => {
  return props.widthLimit - 1 - unaryConnSymbol.value.length;
});

const fWidth = ref<number | null>(null);

function setFWidth(w: number) { fWidth.value = w; }

const unaryTotalWidth = computed(() => {
  return unaryConnSymbol.value.length + 1 + (fWidth.value ?? 0); // 1 for space
});

// when fWidth is set, emit total width
watch(unaryTotalWidth, w => {
  if (props.form?.f.tag == 'FormUnary' && fWidth.value !== null) {
    emitWidth(w);
  }
});

////////////////////////////
// for binary logical connectors
////////////////////////////

const binaryConnSymbol = computed(() => {
  return props.form && props.form.f.tag === 'FormBinary'
    ? binaryConnSymbolMap[props.form.f.bconn]
    : '';
});

const binaryChildWidthLimit = computed(() => {
  // assuming horizontal layout (worst case)
  return props.widthLimit - 1; // 1 for space
});

const f1Width = ref<number | null>(null);
const f2Width = ref<number | null>(null);

function setF1Width(w: number) { f1Width.value = w; }
function setF2Width(w: number) { f2Width.value = w; }

const binaryConnTotalWidthIfHorizontal = computed(() => {
  if (f1Width.value === null || f2Width.value === null) {
    return 0;
  }
  return f1Width.value + f2Width.value + binaryConnSymbol.value.length + 2; // 2 for spaces
});

const binaryConnFitsHorizontal = computed(() => {
  return binaryConnTotalWidthIfHorizontal.value <= props.widthLimit;
});

const binaryConnTotalWidthIfVertical = computed(() => {
  if (f1Width.value === null || f2Width.value === null) {
    return 0;
  }
  return Math.max(f1Width.value, f2Width.value) + 1; // 1 for space
});

// when both f1Width and f2Width are set, emit total width
watch(binaryConnTotalWidthIfHorizontal, w => {
  // binary formula, both children's widths are needed
  if (props.form?.f.tag === 'FormBinary') {
    if (f1Width.value !== null && f2Width.value !== null) {
      if (binaryConnFitsHorizontal.value) {
        // enough horizontal space, using horizontal layout
        emitWidth(binaryConnTotalWidthIfHorizontal.value);
      } else {
        // not enough horizontal space, using vertical layout
        emitWidth(binaryConnTotalWidthIfVertical.value);
      }
    }
  }
});

////////////////////////////
// for if-then-else formulas
////////////////////////////

const iteChildWidthLimit = computed(() => {
  // assuming horizontal layout (worst case)
  return props.widthLimit - 1; // 1 for space
});

const fcWidth = ref<number | null>(null);
const ftWidth = ref<number | null>(null);
const ffWidth = ref<number | null>(null);

function setFcWidth(w: number) { fcWidth.value = w; }
function setFtWidth(w: number) { ftWidth.value = w; }
function setFfWidth(w: number) { ffWidth.value = w; }

const iteTotalWidthIfHorizontal = computed(() => {
  if (fcWidth.value === null || ftWidth.value === null || ffWidth.value === null) {
    return 0;
  }
  return fcWidth.value + ftWidth.value + ffWidth.value + 13;
  // 13 for spaces and keywords "if", "then", "else" (10 chars) + 3 for spaces
});

const iteFitsHorizontal = computed(() => {
  return iteTotalWidthIfHorizontal.value <= props.widthLimit;
});

const iteTotalWidthIfVertical = computed(() => {
  if (fcWidth.value === null || ftWidth.value === null || ffWidth.value === null) {
    return 0;
  }
  return Math.max(fcWidth.value, ftWidth.value, ffWidth.value) + 1; // 1 for space
});

// when fcWidth, ftWidth, and ffWidth are set, emit total width
watch(iteTotalWidthIfHorizontal, w => {
  // if-then-else formula, all children's widths are needed
  if (props.form?.f.tag === 'FormIfThenElse') {
    if (fcWidth.value !== null && ftWidth.value !== null && ffWidth.value !== null) {
      if (iteFitsHorizontal.value) {
        // enough horizontal space, using horizontal layout
        emitWidth(iteTotalWidthIfHorizontal.value);
      } else {
        // not enough horizontal space, using vertical layout
        emitWidth(iteTotalWidthIfVertical.value);
      }
    }
  }
});

// send click event for this expression
function clickedHere(event: MouseEvent) {
  emits('click', { type: "form", formHash: props.form!.hash });
  // prevent event bubbling
  event.stopPropagation();
}

const isHighlighted = computed(() =>
  props.highlightedSubFormExpr?.type == 'form'
  && props.highlightedSubFormExpr.formHash === props.form?.hash);

const highlightedSubExpr = computed(() =>
  props.highlightedSubFormExpr?.type === 'expr' ? props.highlightedSubFormExpr.exprHash : undefined);

const style = computed<StyleValue>(() => {
  return {
    'backgroundColor': colour.value,
    'border': isHighlighted.value ? '1.5px solid blue' : '0.5px solid grey',
  };
});

</script>

<template>
  <span v-if="!form" class="border">
    <em>???</em>
  </span>
  <span v-else @click="clickedHere">
    <!-- literal formulas True / False -->
    <span v-if="form.f.tag === 'FormTrue'" :style="style">{{ emitWidthFromString('True') }}</span>
    <span v-else-if="form.f.tag === 'FormFalse'" :style="style">{{ emitWidthFromString('False') }}</span>
    <!-- comparisons -->
    <span v-else-if="form.f.tag === 'FormComp'" :style="style"
      :class="{ 'd-flex': true, 'flex-column': !binaryCompFitsHorizontal, 'align-items-center': true }">
      <span class="px-1">
        <FormattedExpr :expr="form.f.e1" :widthLimit="binaryCompChildWidthLimit" @width="setE1Width"
          @click="(e) => emits('click', { type: 'expr', exprHash: e })" :highlightedExpr="highlightedSubExpr" />
      </span>
      {{ binaryCompSymbol }}
      <span class="px-1">
        <FormattedExpr :expr="form.f.e2" :widthLimit="binaryCompChildWidthLimit" @width="setE2Width"
          @click="(e) => emits('click', { type: 'expr', exprHash: e })" :highlightedExpr="highlightedSubExpr" />
      </span>
    </span>
    <!-- unary logical connectors -->
    <span v-else-if="form.f.tag === 'FormUnary'" :style="style"
      class="d-flex align-items-center justify-content-center">
      {{ unaryConnSymbol }}
      <span class="px-1">
        <FormattedForm :form="form.f.f1" :formValues="formValues" :widthLimit="unaryChildWidthLimit" @width="setFWidth"
          @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
    </span>
    <!-- binary logical connectors -->
    <span v-else-if="form.f.tag === 'FormBinary'" :style="style"
      :class="{ 'd-flex': true, 'flex-column': !binaryConnFitsHorizontal, 'align-items-center': true }">
      <span class="px-1">
        <FormattedForm :form="form.f.f1" :formValues="formValues" :widthLimit="binaryChildWidthLimit"
          @width="setF1Width" @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
      {{ binaryConnSymbol }}
      <span class="px-1">
        <FormattedForm :form="form.f.f2" :formValues="formValues" :widthLimit="binaryChildWidthLimit"
          @width="setF2Width" @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
    </span>
    <!-- if-then-else formulas -->
    <span v-else-if="form.f.tag === 'FormIfThenElse'" :style="style"
      :class="{ 'd-flex': true, 'flex-column': !iteFitsHorizontal, 'align-items-center': true }">
      <span>If</span>
      <span class="px-1">
        <FormattedForm :form="form.f.fc" :formValues="formValues" :widthLimit="iteChildWidthLimit" @width="setFcWidth"
          @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
      then
      <span class="px-1">
        <FormattedForm :form="form.f.ft" :formValues="formValues" :widthLimit="iteChildWidthLimit" @width="setFtWidth"
          @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
      else
      <span class="px-1">
        <FormattedForm :form="form.f.ff" :formValues="formValues" :widthLimit="iteChildWidthLimit" @width="setFfWidth"
          @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
    </span>
  </span>
</template>
