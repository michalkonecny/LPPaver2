import { defineStore } from 'pinia';
import { reactive, readonly, ref, watch, type DeepReadonly, type Ref } from 'vue';
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

export type RunStatus = 'RequestSent' | 'SolverRunning' | 'SolverFinished';

export type RunInfo = {
  runId: string;
  problemName: string;
  paramValues: Record<string, number>;
  status: RunStatus;
  // TODO: add steps
};

// Haskell definition:
// data Arithmetic
//   = BallArithmetic {precision :: Integer}
//   | AffineArithmetic {precision :: Integer, maxTerms :: Int}

export type Arithmetic =
  | { tag: 'BallArithmetic'; precision: number }
  | { tag: 'AffineArithmetic'; precision: number; maxTerms: number };

export type RunSolverRequest = {
  runId: string;
  problemName: string;
  paramValues: Record<string, number>;
  arithmetic: Arithmetic;
  giveUpAccuracy: number;
  numberOfThreads: number;
};

export const useProverStore = defineStore('prover', () => {
  const exampleProblems: Ref<Record<string, ProblemWithParamSpec>> = ref({});
  const boxes: Ref<Record<BoxHash, Box>> = ref({});
  const exprs: Ref<Record<ExprHash, ExprF<ExprHash>>> = ref({});
  const forms: Ref<Record<FormHash, FormF<ExprHash, FormHash>>> = ref({});
  const runs: Ref<Record<string, RunInfo>> = ref({});
  const currentRunId: Ref<string | null> = ref(null);

  const exports = {
    exampleProblems: readonly(exampleProblems),
    boxes: readonly(boxes),
    exprs: readonly(exprs),
    forms: readonly(forms),
    runs: readonly(runs),
    currentRunId: currentRunId,
    getBox,
    getExpr,
    getForm,
    startRun,
    getRunInfo,
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

  async function startRun(
    problemName: string,
    paramValues: Record<string, number>,
    arithmetic: Arithmetic,
    giveUpAccuracy: number,
    numberOfThreads: number = 4,
  ) {
    const ws = await getProverWS();
    const runId = generateRunId();
    const message: RunSolverRequest = {
      runId,
      problemName,
      paramValues,
      arithmetic,
      giveUpAccuracy,
      numberOfThreads,
    };

    ws.send(JSON.stringify(message));

    runs.value[runId] = reactive({
      runId,
      problemName,
      paramValues,
      status: 'RequestSent',
    });

    currentRunId.value = runId;
  }

  function generateRunId(): string {
    // generate a random 12-character alphanumeric string
    return Math.random().toString(36).substring(2, 14);
  }

  function getRunInfo(runId: string): DeepReadonly<RunInfo> | null {
    const runInfo = runs.value[runId];
    if (!runInfo) return null;
    return readonly(runInfo);
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
        case 'ResponseSolverRunStatusUpdate': {
          const { runId, status } = message.contents;
          if (runs.value[runId]) {
            runs.value[runId].status = status;
          } else {
            console.warn(`Received run status for unknown runId ${runId}`);
          }
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
    }
  | {
      tag: 'ResponseSolverRunStatusUpdate';
      contents: {
        runId: string;
        status: RunStatus;
      };
    };
