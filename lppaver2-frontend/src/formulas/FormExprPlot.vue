<script setup lang="ts">
import { ref } from 'vue';
import { type FormOrExprHash } from './forms';

const props = defineProps<{
  formOrExprHash: FormOrExprHash;
}>();

const plotDiv = ref<HTMLDivElement | null>(null);

</script>

<template>
  <!-- TODO: make plot resize with grid cell resize -->
  <div ref="plotDiv" class="w-100" style="height: 90%;"></div>
</template>