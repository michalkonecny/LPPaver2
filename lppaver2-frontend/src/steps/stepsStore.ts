import { defineStore } from 'pinia'
import type { Box, ExprHash, FormHash, Step } from './steps'
import type { ExprF } from './exprs'
import type { FormF } from './forms'

export const useStepsStore = defineStore('steps', {
  state: () => ({
    sessionRef: null as string | null,
    boxes: {} as Record<string, Box>,
    exprs: {} as Record<string, ExprF<ExprHash>>,
    forms: {} as Record<string, FormF<ExprHash, FormHash>>,
    steps: [] as Step[],
  }),
  actions: {
    async initSession(sessionRef: string) {
      this.sessionRef = sessionRef
      // fetch boxes from redis
      const boxes = await fetchWholeHash<Box>(sessionRef, 'boxes')
      console.log("boxes from redis: ", boxes)
      this.boxes = boxes

      // fetch exprs from redis
      const exprs = await fetchWholeHash<ExprF<ExprHash>>(sessionRef, 'exprs')
      console.log("exprs from redis: ", exprs)
      this.exprs = exprs

      // fetch forms from redis
      const forms = await fetchWholeHash<FormF<ExprHash, FormHash>>(sessionRef, 'forms')
      console.log("forms from redis: ", forms)
      this.forms = forms

      // fetch steps from redis
      const steps = await fetchWholeList<Step>(sessionRef, 'steps')
      console.log("steps from redis: ", steps)
      this.steps = steps
    },
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