import { defineStore } from 'pinia';
import { ref, type Ref } from 'vue';
import _ from 'lodash';
import { getProverWS } from './proverWS';
import type { Problem } from '@/problems/problems';
import type { Box, BoxHash } from '@/boxes/boxes';
import type { ExprF, ExprHash } from '@/formulas/exprs';
import type { FormF, FormHash } from '@/formulas/forms';

export const useProverStore = defineStore('prover', () => {
  const exampleProblems: Ref<Problem[]> = ref([]);
  const boxes: Ref<Record<BoxHash, Box>> = ref({});
  const exprs: Ref<Record<ExprHash, ExprF<ExprHash>>> = ref({});
  const forms: Ref<Record<FormHash, FormF<ExprHash, FormHash>>> = ref({});

  const exports = {
    exampleProblems,
    boxes,
    exprs,
    forms,
    requestExampleProblems,
    requestAllFormulaNodes,
  };

  async function requestExampleProblems() {
    const ws = await getProverWS();
    ws.send(JSON.stringify('GetExampleProblemsRequest'));
  }

  async function requestAllFormulaNodes() {
    const ws = await getProverWS();
    ws.send(JSON.stringify('GetAllFormulaNodesRequest'));
  }

  //////////////////////////////////////////
  // Updating state based on prover messages
  //////////////////////////////////////////

  async function _watchProverMessages() {
    const ws = await getProverWS();
    ws.addEventListener('message', (ws, event) => {
      // console.log(`ws message event:`, event);

      const message: ProverMessage = JSON.parse(event.data);
      console.log(`ws message:`, message);
      switch (message.tag) {
        case 'ResponseExampleProblems': {
          exampleProblems.value = message.contents.problems;
          boxes.value = { ...boxes.value, ...message.contents.boxes };
          break;
        }
        case 'ResponseFormulaNodes': {
          exprs.value = { ...exprs.value, ...message.contents.exprs };
          forms.value = { ...forms.value, ...message.contents.forms };
          break;
        }
        default:
          console.warn('Unrecognised message from prover backend:', message);
      }
    });
  }

  // start watching for messages from the prover backend
  _watchProverMessages();

  return exports;
});

type ProverMessage =
  | {
      tag: 'ResponseExampleProblems';
      contents: {
        problems: Problem[];
        boxes: Record<BoxHash, Box>;
      };
    }
  | {
      tag: 'ResponseFormulaNodes';
      contents: {
        exprs: Record<ExprHash, ExprF<ExprHash>>;
        forms: Record<FormHash, FormF<ExprHash, FormHash>>;
      };
    };
