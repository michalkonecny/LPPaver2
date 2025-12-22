<script lang="ts" setup>
import { binaryCompSymbolMap, binaryConnSymbolMap, unaryConnSymbolMap, type Form } from './forms';
import FormattedExpr from './FormattedExpr.vue';

const props = defineProps<{
  form?: Form;
}>();

</script>

<template>
  <span v-if="!form" class="border">
    <em>???</em>
  </span>
  <span v-if="form" class="border">
    <span v-if="form.f.tag === 'FormTrue'">True</span>
    <span v-else-if="form.f.tag === 'FormFalse'">False</span>
    <span v-else-if="form.f.tag === 'FormComp'" class="d-flex align-items-center justify-content-center">
      <FormattedExpr :expr="form.f.e1" />
      {{ binaryCompSymbolMap[form.f.comp] }}
      <FormattedExpr :expr="form.f.e2" />
    </span>
    <span v-else-if="form.f.tag === 'FormUnary'" class="d-flex align-items-center justify-content-center">
      {{ unaryConnSymbolMap[form.f.uconn] }}
      <FormattedForm :form="form.f.f1" />
    </span>
    <span v-else-if="form.f.tag === 'FormBinary'" class="d-flex align-items-center justify-content-center">
      <FormattedForm :form="form.f.f1" />
      {{ binaryConnSymbolMap[form.f.bconn] }}
      <FormattedForm :form="form.f.f2" />
    </span>
    <span v-else-if="form.f.tag === 'FormIfThenElse'" class="d-flex align-items-center justify-content-center">
      If
      <FormattedForm :form="form.f.fc" />
      then
      <FormattedForm :form="form.f.ft" />
      else
      <FormattedForm :form="form.f.ff" />
    </span>
  </span>
</template>
