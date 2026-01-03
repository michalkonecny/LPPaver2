<script lang="ts" setup>
import { computed, ref, watch, type Ref } from 'vue';
import { binaryOpSymbolMap, unaryOpSymbolMap, type Expr, type RationalLit } from './exprs';

const props = defineProps<{
  expr?: Expr;
  widthLimit: number;
}>();

const emits = defineEmits<{
  (e: 'width', width: number): void;
}>();

////////////////////////////
// for literals and variables
////////////////////////////

function emitWidth(width: number) {
  emits('width', width);
}

function emitWidthFromString(result: string) {
  emitWidth(result.length);
  return result;
}

function formatLit(lit: RationalLit): string {
  const x = lit.numerator / lit.denominator;
  return x.toLocaleString(undefined, { maximumFractionDigits: 5 });
}

////////////////////////////
// for unary operators
////////////////////////////

const unaryOpSymbol = computed(() => {
  return props.expr && props.expr.e.tag === 'ExprUnary'
    ? unaryOpSymbolMap[props.expr.e.unop]
    : '';
});

const unaryChildWidthLimit = computed(() => {
  return props.widthLimit - 1 - unaryOpSymbol.value.length;
});

const eWidth = ref<number | null>(null);

function setEWidth(w: number) { eWidth.value = w; }

const unaryTotalWidth = computed(() => {
  return unaryOpSymbol.value.length + 1 + (eWidth.value ?? 0); // 1 for space
});

// when eWidth is set, emit total width
watch(unaryTotalWidth, w => {
  if (props.expr?.e.tag == 'ExprUnary' && eWidth.value !== null) {
    // unary expression, the child's width is known, can output its total width
    emitWidth(w);
  }
});

////////////////////////////
// for binary operators
////////////////////////////

const binaryOpSymbol = computed(() => {
  return props.expr && props.expr.e.tag === 'ExprBinary'
    ? binaryOpSymbolMap[props.expr.e.binop]
    : '';
});

const binaryChildWidthLimit = computed(() => {
  // assuming horizontal layout (worst case)
  return props.widthLimit - 1; // 1 for space
});

const e1Width = ref<number | null>(null);
const e2Width = ref<number | null>(null);

function setE1Width(w: number) { e1Width.value = w; }
function setE2Width(w: number) { e2Width.value = w; }

const binaryTotalWidthIfHorizontal = computed(() => {
  if (e1Width.value === null || e2Width.value === null) {
    return 0;
  }
  return e1Width.value + e2Width.value + binaryOpSymbol.value.length + 2; // 2 for spaces
});

const binaryTotalWidthIfVertical = computed(() => {
  if (e1Width.value === null || e2Width.value === null) {
    return 0;
  }
  return Math.max(e1Width.value, e2Width.value) + 1; // 1 for space
});

const binaryFitsHorizontal = computed(() => {
  return binaryTotalWidthIfHorizontal.value <= props.widthLimit;
});

// when both e1Width and e2Width are set, emit total width
watch(binaryTotalWidthIfHorizontal, w => {
  // binary expression, both children's widths are needed
  if (props.expr?.e.tag === 'ExprBinary') {
    if (e1Width.value !== null && e2Width.value !== null) {
      if (binaryFitsHorizontal.value) {
        // enough horizontal space, using horizontal layout
        emitWidth(binaryTotalWidthIfHorizontal.value);
      } else {
        // not enough horizontal space, using vertical layout
        emitWidth(binaryTotalWidthIfVertical.value);
      }
    }
  }
});

</script>

<template>
  <span v-if="!expr" class="border">
    <em>???</em>
  </span>
  <span v-if="expr">
    <!-- literals -->
    <span v-if="expr.e.tag === 'ExprLit'">
      {{ emitWidthFromString(formatLit(expr.e.lit)) }}
    </span>
    <!-- variables -->
    <span v-else-if="expr.e.tag === 'ExprVar'">
      {{ emitWidthFromString(expr.e.var) }}
    </span>
    <!-- unary operators -->
    <span v-else-if="expr.e.tag === 'ExprUnary'" class="d-flex align-items-center justify-content-center">
      {{ unaryOpSymbol }}
      <span class="border px-1">
        <FormattedExpr :expr="expr.e.e1" :widthLimit="unaryChildWidthLimit" @width="setEWidth" />
      </span>
    </span>
    <!-- binary operators -->
    <span v-else-if="expr.e.tag === 'ExprBinary'"
      :class="{ 'd-flex': true, 'flex-column': !binaryFitsHorizontal, 'align-items': true }">
      <span class="border px-1">
        <FormattedExpr :expr="expr.e.e1" :widthLimit="binaryChildWidthLimit" @width="setE1Width" />
      </span>
      {{ binaryOpSymbolMap[expr.e.binop] }}
      <span class="border px-1">
        <FormattedExpr :expr="expr.e.e2" :widthLimit="binaryChildWidthLimit" @width="setE2Width" />
      </span>
    </span>
  </span>
</template>
