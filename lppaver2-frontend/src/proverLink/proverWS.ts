///////////////////////////////////////////////
// Websocket connection to the prover backend
///////////////////////////////////////////////

import { ref, type Ref } from 'vue';
import { WebsocketBuilder, type Websocket } from 'websocket-ts';

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
export async function getProverWS() {
  // This loop executes only when an action is called before the websocket connection
  // is established, which is unlikely but possible.
  // In that case, we poll until the connection is ready before proceeding.
  while (!_ws.value) {
    await new Promise((resolve) => setTimeout(resolve, 100));
  }
  return _ws.value;
}
