import { defineStore } from 'pinia';
import { ref, type Ref } from 'vue';
import _ from 'lodash';
import { Websocket, WebsocketBuilder } from 'websocket-ts';
import type { Problem } from '@/problems/problems';

export const useProverStore = defineStore('prover', () => {
  const exampleProblems: Ref<Problem[]> = ref([]);

  const exports = {
    exampleProblems,
    requestExampleProblems,
  };

  async function requestExampleProblems() {
    const ws = await _getWS();
    ws.send(JSON.stringify({ type: 'GetExampleProblems' }));
  }

  ///////////////////////////////////////////////
  // Websocket connection to the prover backend
  ///////////////////////////////////////////////

  /** either null or an active Websocket connection */
  const _ws: Ref<Websocket | null> = ref(null);

  /** initialize the websocket connection to the prover backend */
  async function _initProverWS() {
    // start a websocket connection to the prover backend
    const ws = new WebsocketBuilder('ws://localhost:9160').build();
    // wait until connection is open before proceeding
    await new Promise<void>((resolve) => {
      ws.addEventListener('open', () => resolve());
    });
    _ws.value = ws;
  }

  // immediately initialize the websocket connection when the store is created
  _initProverWS();

  /** wait until the websocket connection is established and return it */
  async function _getWS() {
    // This loop executes only when an action is called before the websocket connection
    // is established, which is unlikely but possible.
    // In that case, we poll until the connection is ready before proceeding.
    while (!_ws.value) {
      await new Promise((resolve) => setTimeout(resolve, 100));
    }
    return _ws.value;
  }

  //////////////////////////////////////////
  // Updating state based on prover messages
  //////////////////////////////////////////

  async function _watchProverMessages() {
    const ws = await _getWS();
    ws.addEventListener('message', (ws, event) => {
      // console.log(`ws message event:`, event);

      const message = JSON.parse(event.data);
      console.log(`ws message:`, message);
      switch (message.tag) {
        case 'ResponseExampleProblems': {
          exampleProblems.value = message.contents.problems;
          break;
        }
        default:
          console.warn('Unknown message type from prover backend:', message.tag);
      }
    });
  }

  // start watching for messages from the prover backend
  _watchProverMessages();

  return exports;
});

type ProverMessage = {
  tag: 'ResponseExampleProblems' | string;
  contents: {
    problems: Problem[];
  };
};
