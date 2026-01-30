<script setup lang="ts">
import { computed } from 'vue';
import type { Box } from '@/steps/steps';

const props = defineProps<{
  box: Box;
}>();

const variableRangeStrs = computed(() => {
  const varDomains = props.box.box_.varDomains ?? [];
  const parts = Object.entries(varDomains).map(([v, d]) => {
    return { l: d.l, v, u: d.u };
  });
  return parts;
});

</script>

<template>
  <div class="d-flex justify-content-center">
    <table class="table table-sm w-auto">
      <tbody>
        <tr v-for="varRange in variableRangeStrs" :key="varRange.v">
          <td class="text-end">{{ varRange.l }}</td>
          <td>≤</td>
          <td class="text-center">{{ varRange.v }}</td>
          <td>≤</td>
          <td class="text-start">{{ varRange.u }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>