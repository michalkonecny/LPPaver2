import { defineStore } from 'pinia';
import { readonly, ref, watch, type Ref } from 'vue';
import _ from 'lodash';
import { getProverWS } from './proverWS';
import type { Problem } from '@/problems/problems';
import type { Box, BoxHash } from '@/boxes/boxes';
import { exprHashToExpr, type Expr, type ExprF, type ExprHash } from '@/formulas/exprs';
import { formHashToForm, type Form, type FormF, type FormHash } from '@/formulas/forms';

export type ProblemWithParamSpec = {
  problem: Problem;
  paramSpecs: ParamSpec[];
};

export type ParamSpec = {
  paramName: string;
  defaultValue: number;
  minValue: number;
  maxValue: number;
};

export const useProverStore = defineStore('prover', () => {
  const exampleProblems: Ref<Record<string, ProblemWithParamSpec>> = ref({});
  const boxes: Ref<Record<BoxHash, Box>> = ref({});
  const exprs: Ref<Record<ExprHash, ExprF<ExprHash>>> = ref({});
  const forms: Ref<Record<FormHash, FormF<ExprHash, FormHash>>> = ref({});

  const exports = {
    exampleProblems: readonly(exampleProblems) as typeof exampleProblems,
    getBox,
    getExpr,
    getForm,
  };

  function getExpr(exprHash: ExprHash): Expr {
    return exprHashToExpr(exprHash, exprs.value);
  }

  function getForm(formHash: FormHash): Form {
    return formHashToForm(formHash, forms.value, exprs.value);
  }

  function getBox(boxHash: BoxHash): Box {
    const box = boxes.value[boxHash];
    if (!box) {
      console.log(`boxes.value = `, boxes.value);
      console.log(`typeof(boxHash) = `, typeof boxHash);

      throw new Error(`Box with hash ${boxHash} not found`);
    }
    return box;
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

  /////////////////////////
  // initialise the store
  /////////////////////////

  // whenever exampleProblems is assigned, request all formula nodes
  watch(exampleProblems, async () => {
    const ws = await getProverWS();
    ws.send(JSON.stringify('GetAllFormulaNodesRequest'));
  });

  // request example problems on store initialisation
  requestExampleProblems();

  async function requestExampleProblems() {
    const ws = await getProverWS();
    ws.send(JSON.stringify('GetExampleProblemsRequest'));
  }

  return exports;
});

type ProverMessage =
  | {
      tag: 'ResponseExampleProblems';
      contents: {
        problems: Record<string, ProblemWithParamSpec>;
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
