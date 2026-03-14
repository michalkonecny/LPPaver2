<script lang="ts" setup>
import _ from 'lodash';
import { binaryCompSymbolMap, binaryConnSymbolMap, unaryConnSymbolMap, type Form, type FormOrExprHash } from './forms';
import { computed, ref, watch, type StyleValue } from 'vue';
import { getTruthColour } from '@/steps/stepsStore';
import FormattedExpr from './FormattedExpr.vue';
import type { FormValues } from './evalInfo';

const props = defineProps<{
  form?: Form;
  formValues?: FormValues;
  inInfo: InInfo;
  highlightedSubFormExpr?: FormOrExprHash;
}>();

/**
 ```
  +------------------------+
  |         other          |
  | +-------------+        |     
  | |  DIE        |        |  
  | |      +------+------+ |
  | |      |  IE  |      | |
  | +------+------+      | |
  |        |        CIE  | |
  |        +-------------+ |
  +------------------------+
```
*/
type IEFormType =
  "IE" | // inequality
  "CIE" | // conjunction of inequalities (including ==)
  "DIE" | // disjunction of inequalities (including !=)
  "Other";

type InInfo = { 
  widthLimit: number;
  couldBeFormType: IEFormType;
}

type OutInfo = {
  width: number;
  formType: IEFormType;
}

const outInfo = ref<OutInfo>();

const formType = computed<IEFormType>(() => {
  if(!outInfo.value) return 'Other';
  
  const couldBeFormType = props.inInfo.couldBeFormType;
  const myType = outInfo.value.formType;
  
  // the most specific type
  if (couldBeFormType === 'IE' && myType === 'IE')  return "IE";
    
  const couldBeCIE = _.includes(['CIE', 'IE'], couldBeFormType);
  const isCIE = _.includes(['CIE', 'IE'], myType);
  if (couldBeCIE && isCIE) return "CIE";

  const couldBeDIE = _.includes(['DIE', 'IE'], couldBeFormType);
  const isDIE = _.includes(['DIE', 'IE'], myType);
  if (couldBeDIE && isDIE) return "DIE";

  return 'Other';
})

const emits = defineEmits<{
  (e: 'outInfo', info: OutInfo): void;
  (e: 'click', data: FormOrExprHash): void;
}>();

function emitOutInfo(info: OutInfo) {
  emits('outInfo', info);
  outInfo.value = info;
}

function emitOutInfoFromString(result: string) {
  emitOutInfo({ width: result.length, formType: "Other" });
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
  return props.inInfo.widthLimit - 1; // 1 for space
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
  return binaryCompTotalWidthIfHorizontal.value <= props.inInfo.widthLimit;
});

// when both e1Width and e2Width are set, emit total width
watch(binaryCompTotalWidthIfHorizontal, w => {
  // binary comparison, both children's widths are needed
  if (props.form?.f.tag === 'FormComp') {
    if (e1Width.value !== null && e2Width.value !== null) {
      const totalWidth = binaryCompFitsHorizontal.value
        ? binaryCompTotalWidthIfHorizontal.value // enough horizontal space, using horizontal layout
        : binaryCompTotalWidthIfVertical.value; // not enough horizontal space, using vertical layout

      const comp = props.form.f.comp;
      emitOutInfo({
        width: totalWidth,
        formType:
          comp == "CompEq" ? 'CIE' // == is a conjunction of two inequalities
            : comp == "CompNeq" ? 'DIE' // != is a disjunction of two inequalities
              : 'IE', // < or <= is a single inequality
      });
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

const unaryChildInInfo = computed<InInfo>(() => {
  return {
    widthLimit: props.inInfo.widthLimit - 1 - unaryConnSymbol.value.length,
    couldBeFormType: 'Other',
  };
});

const fOutInfo = ref<OutInfo | null>(null);

function setFOutInfo(info: OutInfo) { fOutInfo.value = info; }

const unaryTotalWidth = computed(() => {
  return unaryConnSymbol.value.length + 1 + (fOutInfo.value?.width ?? 0); // 1 for space
});

// when fOutInfo is set, emit our out info with the total width
watch(unaryTotalWidth, w => {
  if (props.form?.f.tag == 'FormUnary' && fOutInfo.value !== null) {
    emitOutInfo({
      width: w,
      formType: 'Other', // a unary formula is not an inequality, or conjunction/disjuction of inequalities
    });
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

const binaryChildInInfo = computed<InInfo>(() => {
  // assuming horizontal layout (worst case)
  const connSymbol = binaryConnSymbol.value;
  const myCouldBeFormType = props.inInfo.couldBeFormType;
  const couldByCIE = _.includes(['CIE', 'IE'], myCouldBeFormType) && connSymbol === '∧';
  const couldByDIE = _.includes(['DIE', 'IE'], myCouldBeFormType) && connSymbol === '∨';
  return {
    widthLimit: props.inInfo.widthLimit - 1, // 1 for space
    couldBeFormType: couldByCIE ? 'CIE' : couldByDIE ? 'DIE' : 'Other',
  };
});

const f1OutInfo = ref<OutInfo | null>(null);
const f2OutInfo = ref<OutInfo | null>(null);

function setF1OutInfo(info: OutInfo) { f1OutInfo.value = info; }
function setF2OutInfo(info: OutInfo) { f2OutInfo.value = info; }

const binaryConnTotalWidthIfHorizontal = computed(() => {
  if (f1OutInfo.value === null || f2OutInfo.value === null) {
    return 0;
  }
  return f1OutInfo.value.width + f2OutInfo.value.width + binaryConnSymbol.value.length + 2; // 2 for spaces
});

const binaryConnFitsHorizontal = computed(() => {
  return binaryConnTotalWidthIfHorizontal.value <= props.inInfo.widthLimit;
});

const binaryConnTotalWidthIfVertical = computed(() => {
  if (f1OutInfo.value === null || f2OutInfo.value === null) {
    return 0;
  }
  return Math.max(f1OutInfo.value.width, f2OutInfo.value.width) + 1; // 1 for space
});

// when both f1OutInfo and f2OutInfo are set, emit out info with the total width and form type
watch(binaryConnTotalWidthIfHorizontal, w => {
  // binary formula, both children's widths are needed
  if (props.form?.f.tag === 'FormBinary') {
    if (f1OutInfo.value !== null && f2OutInfo.value !== null) {
      const totalWidth = binaryConnFitsHorizontal.value
        ? binaryConnTotalWidthIfHorizontal.value // enough horizontal space, using horizontal layout
        : binaryConnTotalWidthIfVertical.value; // not enough horizontal space, using vertical layout

      const f1IsCIE = _.includes(['CIE', 'IE'], f1OutInfo.value.formType);
      const f2IsCIE = _.includes(['CIE', 'IE'], f2OutInfo.value.formType);

      const f1IsDIE = _.includes(['DIE', 'IE'], f1OutInfo.value.formType);
      const f2IsDIE = _.includes(['DIE', 'IE'], f2OutInfo.value.formType);

      const formType: IEFormType =
        props.form.f.bconn == "ConnAnd" && f1IsCIE && f2IsCIE ? 'CIE' // conjunction of inequalities
          : props.form.f.bconn == "ConnOr" && f1IsDIE && f2IsDIE ? 'DIE' // disjunction of inequalities
            : 'Other';

      emitOutInfo({
        width: totalWidth,
        formType,
      });
    }
  }
});

////////////////////////////
// for if-then-else formulas
////////////////////////////

const iteChildInInfo = computed<InInfo>(() => {
  // assuming horizontal layout (worst case)
  return { 
    widthLimit: props.inInfo.widthLimit - 1, // 1 for space
    couldBeFormType: 'Other'
   };
});

const fcOutInfo = ref<OutInfo | null>(null);
const ftOutInfo = ref<OutInfo | null>(null);
const ffOutInfo = ref<OutInfo | null>(null);

function setFcOutInfo(info: OutInfo) { fcOutInfo.value = info; }
function setFtOutInfo(info: OutInfo) { ftOutInfo.value = info; }
function setFfOutInfo(info: OutInfo) { ffOutInfo.value = info; }

const iteTotalWidthIfHorizontal = computed(() => {
  if (fcOutInfo.value === null || ftOutInfo.value === null || ffOutInfo.value === null) {
    return 0;
  }
  return fcOutInfo.value.width + ftOutInfo.value.width + ffOutInfo.value.width + 13;
  // 13 for spaces and keywords "if", "then", "else" (10 chars) + 3 for spaces
});

const iteFitsHorizontal = computed(() => {
  return iteTotalWidthIfHorizontal.value <= props.inInfo.widthLimit;
});

const iteTotalWidthIfVertical = computed(() => {
  if (fcOutInfo.value === null || ftOutInfo.value === null || ffOutInfo.value === null) {
    return 0;
  }
  return Math.max(fcOutInfo.value.width, ftOutInfo.value.width, ffOutInfo.value.width) + 1; // 1 for space
});

// when fcOutInfo, ftOutInfo, and ffOutInfo are set, emit out info with the total width and form type
watch(iteTotalWidthIfHorizontal, w => {
  // if-then-else formula, all children's widths are needed
  if (props.form?.f.tag === 'FormIfThenElse') {
    if (fcOutInfo.value !== null && ftOutInfo.value !== null && ffOutInfo.value !== null) {

      const totalWidth = iteFitsHorizontal.value
        ? iteTotalWidthIfHorizontal.value // enough horizontal space, using horizontal layout
        : iteTotalWidthIfVertical.value; // not enough horizontal space, using vertical layout

      emitOutInfo({
        width: totalWidth,
        formType: 'Other', // if-then-else formulas are neither CIEs or DIEs
      });
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

const isTopLevelComparison = computed(() => props.form?.f.tag === 'FormComp' && formType.value !== 'Other');

const style = computed<StyleValue>(() => {
  return {
    'backgroundColor': colour.value,
    'border': isTopLevelComparison.value 
      ? isHighlighted.value ? '2.5px dashed blue' : '2.5px dashed black'
      : isHighlighted.value ? '1.5px solid blue' : '0.5px solid grey',
  };
});

</script>

<template>
  <span v-if="!form" class="border">
    <em>???</em>
  </span>
  <span v-else @click="clickedHere">
    <!-- literal formulas True / False -->
    <span v-if="form.f.tag === 'FormTrue'" :style="style">{{ emitOutInfoFromString('True') }}</span>
    <span v-else-if="form.f.tag === 'FormFalse'" :style="style">{{ emitOutInfoFromString('False') }}</span>
    <!-- comparisons -->
    <span v-else-if="form.f.tag === 'FormComp'" :style="style"
      :class="{ 'd-flex': true, 'flex-column': !binaryCompFitsHorizontal, 'align-items-center': true }">
      <span class="px-1">
        <FormattedExpr :expr="form.f.e1" :widthLimit="binaryCompChildWidthLimit" @width="setE1Width"
          @click="(e) => emits('click', { type: 'expr', exprHash: e })" :highlightedExpr="highlightedSubExpr" />
      </span>
      <span :class="{ 'fw-bold': formType != 'Other' }">{{ binaryCompSymbol }}</span>
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
        <FormattedForm :form="form.f.f1" :formValues="formValues" :inInfo="unaryChildInInfo"
          @outInfo="setFOutInfo" @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
    </span>

    <!-- binary logical connectors -->
    <span v-else-if="form.f.tag === 'FormBinary'" :style="style"
      :class="{ 'd-flex': true, 'flex-column': !binaryConnFitsHorizontal, 'align-items-center': true }">
      <span class="px-1">
        <FormattedForm :form="form.f.f1" :formValues="formValues" :inInfo="binaryChildInInfo"
          @outInfo="setF1OutInfo" @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
      <span :class="{ 'fw-bold': formType != 'Other' }">
        {{ binaryConnSymbol }}
      </span>
      <span class="px-1">
        <FormattedForm :form="form.f.f2" :formValues="formValues" :inInfo="binaryChildInInfo"
          @outInfo="setF2OutInfo" @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
    </span>

    <!-- if-then-else formulas -->
    <span v-else-if="form.f.tag === 'FormIfThenElse'" :style="style"
      :class="{ 'd-flex': true, 'flex-column': !iteFitsHorizontal, 'align-items-center': true }">
      <span>If</span>
      <span class="px-1">
        <FormattedForm :form="form.f.fc" :formValues="formValues" :inInfo="iteChildInInfo"
          @outInfo="setFcOutInfo" @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
      then
      <span class="px-1">
        <FormattedForm :form="form.f.ft" :formValues="formValues" :inInfo="iteChildInInfo"
          @outInfo="setFtOutInfo" @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
      else
      <span class="px-1">
        <FormattedForm :form="form.f.ff" :formValues="formValues" :inInfo="iteChildInInfo"
          @outInfo="setFfOutInfo" @click="(d) => emits('click', d)" :highlightedSubFormExpr="highlightedSubFormExpr" />
      </span>
    </span>
  </span>
</template>
