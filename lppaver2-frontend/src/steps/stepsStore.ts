import { defineStore } from 'pinia'
import { getStepProblem, problemToProblemHash, type Box, type BoxHash, type Problem, type ProblemHash, type Step } from './steps'
import { type ExprHash, type Expr, type ExprF, exprHashToExpr } from './exprs'
import { formHashToForm, type Form, type FormF, type FormHash } from './forms'

export const useStepsStore = defineStore('steps', {
  state: () => ({
    sessionRef: null as string | null,
    boxes: {} as Record<BoxHash, Box>,
    exprs: {} as Record<ExprHash, ExprF<ExprHash>>,
    forms: {} as Record<FormHash, FormF<ExprHash, FormHash>>,
    formTrueHash: null as FormHash | null,
    formFalseHash: null as FormHash | null,
    steps: [] as Step[],
    numberOfSteps: 0, // keep steps separately to make it easier to define reactive dependencies
    _problem2step: {} as Record<ProblemHash, Step>,
    rootProblem: null as Problem | null,
    focusedProblem: null as Problem | null,
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

      // get formTrueHash and formFalseHash from forms
      Object.entries(forms).forEach(([formHash, form]) => {
        if (form.tag === "FormTrue") {
          this.formTrueHash = formHash
        }
        if (form.tag === "FormFalse") {
          this.formFalseHash = formHash
        }
      });

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
    getStepTruthResult(step: Step): "true" | "false" | "unknown" {
      switch (step.tag) {
        case "PruneStep":
          const stepScope = step.problem.scope;
          const inner = step.prunePaving.inner;
          const outer = step.prunePaving.outer;

          // check if the pruned paving's inner or outer cover the whole step scope
          if (inner && inner.boxes[0] == stepScope) return "true";
          if (outer && outer.boxes[0] == stepScope) return "false";
          console.log("step", step);
          
          return "unknown";
        default:
          return "unknown";
      }
    }

  },
  getters: {
  },
})

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
    Object.entries(hgetallResult).map(([boxHash, value]: [string, string]) => [
      boxHash,
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