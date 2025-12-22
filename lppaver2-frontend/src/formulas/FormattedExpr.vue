<script lang="ts" setup>
import { ref, type Ref } from 'vue';
import { binaryOpSymbolMap, unaryOpSymbolMap, type Expr, type RationalLit } from './exprs';

const props = defineProps<{
  expr?: Expr;
  // widthOut: Ref<number | null>;
}>();

// const e1Width = ref<number | null>(null);
// const e2Width = ref<number | null>(null);

function formatLit(lit: RationalLit): string {
  const x = lit.numerator / lit.denominator;
  return x.toLocaleString(undefined, { maximumFractionDigits: 5 });
}

</script>

<template>
  <span v-if="!expr" class="border">
    <em>???</em>
  </span>
  <span v-if="expr">
    <span v-if="expr.e.tag === 'ExprLit'">
      {{ formatLit(expr.e.lit) }}
    </span>
    <span v-else-if="expr.e.tag === 'ExprVar'">
      {{ expr.e.var }}
    </span>
    <span v-else-if="expr.e.tag === 'ExprUnary'" class="d-flex align-items-center justify-content-center">
      {{ unaryOpSymbolMap[expr.e.unop] }}
      <span class="border">
        <FormattedExpr :expr="expr.e.e1" />
      </span>
    </span>
    <span v-else-if="expr.e.tag === 'ExprBinary'" class="d-flex align-items-center justify-content-center">
      <span class="border">
        <FormattedExpr :expr="expr.e.e1" />
      </span>
      {{ binaryOpSymbolMap[expr.e.binop] }}
      <span class="border">
        <FormattedExpr :expr="expr.e.e2" />
      </span>
    </span>
  </span>
</template>
