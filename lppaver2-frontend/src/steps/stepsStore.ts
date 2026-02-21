import { defineStore } from 'pinia'
import {
  getStepProblem, problemToProblemHash, type Box, type BoxHash,
  type Problem, type ProblemHash, type Step
} from './steps'
import { type ExprHash, type Expr, type ExprF, exprHashToExpr } from '../formulas/exprs'
import { formHashToForm, type Form, type FormF, type FormHash, type FormOrExprHash } from '../formulas/forms'
import type { Kleenean } from '@/formulas/kleenean'
import type { ExprValue } from '@/formulas/evalInfo'

export const useStepsStore = defineStore('steps', {
  state: () => ({
    sessionRef: null as string | null,
    boxes: {} as Record<BoxHash, Box>,
    exprs: {} as Record<ExprHash, ExprF<ExprHash>>,
    forms: {} as Record<FormHash, FormF<ExprHash, FormHash>>,
    steps: [] as Step[],
    numberOfSteps: 0, // keep steps separately to make it easier to define reactive dependencies
    _problem2step: {} as Record<ProblemHash, Step>,
    rootProblem: null as Problem | null,
    focusedProblem: null as Problem | null,
    focusedProblemSubFormExpr: null as FormOrExprHash | null, // set in App.vue when user clicks on a sub-form or sub-expr
    zoomedProblem: null as Problem | null,
  }),
  actions: {
    async initSession(sessionRef: string) {
      this.sessionRef = sessionRef
      // fetch boxes from redis
      const boxes = await fetchWholeHash<Box>(sessionRef, 'boxes')
      this.boxes = boxes

      // fetch exprs from redis
      const exprs = await fetchWholeHash<ExprF<ExprHash>>(sessionRef, 'exprs')
      this.exprs = exprs

      // fetch forms from redis
      const forms = await fetchWholeHash<FormF<ExprHash, FormHash>>(sessionRef, 'forms')
      this.forms = forms

      // fetch steps from redis
      const steps = await fetchWholeList<Step>(sessionRef, 'steps')
      this.steps = steps
      this.numberOfSteps = steps.length

      // populate the stepFromProblem map
      steps.forEach(step => {
        const problem = getStepProblem(step);
        if (!problem) return; // continue the loop if no problem
        const problemHash = problemToProblemHash(problem);
        this._problem2step[problemHash] = step;
      });

      this.rootProblem = steps[0] ? getStepProblem(steps[0]) : null;
      this.zoomedProblem = this.rootProblem;
      this.focusedProblem = this.rootProblem;
    },
    getExpr(exprHash: ExprHash): Expr {
      return exprHashToExpr(exprHash, this.exprs)
    },
    getForm(formHash: FormHash): Form {
      return formHashToForm(formHash, this.forms, this.exprs)
    },
    getBox(boxHash: BoxHash): Box {
      const box = this.boxes[boxHash]
      if (!box) {
        console.log(`this.boxes = `, this.boxes);
        console.log(`typeof(boxHash) = `, typeof (boxHash));

        throw new Error(`Box with hash ${boxHash} not found`)
      }
      return box
    },
    stepFromProblem(p: Problem) {
      const problemHash = problemToProblemHash(p);
      const step = this._problem2step[problemHash];
      if (!step) {
        throw new Error(`Step not found for problem hash ${problemHash}`);
      }
      return step;
    },
    getStepTruthResult(step: Step): Kleenean {
      switch (step.tag) {
        case "ProgressStep":
          const stepScope = step.problem.scope;
          const inner = step.progressPaving.inner;
          const outer = step.progressPaving.outer;

          // check if the pruned paving's inner or outer cover the whole step scope
          if (inner && inner.boxes[0] == stepScope) return "CertainTrue";
          if (outer && outer.boxes[0] == stepScope) return "CertainFalse";

          return "TrueOrFalse";
        default:
          return "TrueOrFalse";
      }
    },
    getStepColour(step: Step) {
      if (step.tag === "GiveUpOnProblemStep") {
        return "#ffffff";
        // return "#f0b0f0";
      }

      const truthResult = this.getStepTruthResult(step);
      return getTruthColour(truthResult);
    }
  },
  getters: {
    focusedExprValues(state): Record<ExprHash, ExprValue> | undefined {
      if (!state.focusedProblem) return undefined;
      const step = state._problem2step[problemToProblemHash(state.focusedProblem)];
      if (!step || step.tag !== "ProgressStep") return undefined;
      return step.evalInfo.exprValues;
    }
  },
})

export function getTruthColour(kleenean: Kleenean): string {
  switch (kleenean) {
    case "CertainTrue":
      return "#e0ffe0";
    case "CertainFalse":
      return "#ffd0e0";
    case "TrueOrFalse":
      return "#e0e0ff";
  }
}

const keyPrefix = 'lppaver2'
const wedisURLbase = 'http://127.0.0.1:7379'

function getSessionKeyPrefix(sessionRef: string) {
  return `${keyPrefix}:${sessionRef}`
}

async function fetchWholeHash<ValueType>(sessionRef: string, hashKey: string) {
  const key = `${getSessionKeyPrefix(sessionRef)}:${hashKey}`
  const response = await fetch(`${wedisURLbase}/HGETALL/${key}`).catch(() => {
    throw new Error(`Failed to fetch hash ${hashKey} from redis`)
  })

  // parse response
  const data = await response.json()
  // check response contains HGETALL field with an object value
  if (!("HGETALL" in data) || typeof data.HGETALL !== 'object') {
    throw new Error('Invalid response from redis when fetching a hash')
  }
  // parse the values as ValueType
  const hgetallResult = data.HGETALL as Record<string, string>
  const values = Object.fromEntries(
    Object.entries(hgetallResult).map(([valueHash, value]: [string, string]) => [
      valueHash,
      JSON.parse(value) as ValueType,
    ])
  )
  return values as Record<string, ValueType>
}

async function fetchWholeList<ValueType>(sessionRef: string, listKey: string) {
  const key = `${getSessionKeyPrefix(sessionRef)}:${listKey}`
  const response = await fetch(`${wedisURLbase}/LRANGE/${key}/0/-1`).catch(() => {
    throw new Error(`Failed to fetch list ${listKey} from redis`)
  })

  // parse response
  const data = await response.json()
  // check response contains LRANGE field with an array value
  if (!("LRANGE" in data) || !Array.isArray(data.LRANGE)) {
    throw new Error('Invalid response from redis when fetching a list')
  }
  // parse the values as ValueType
  const lrangeResult = data.LRANGE as string[]
  const values = lrangeResult.map((value) => JSON.parse(value) as ValueType)
  return values
}