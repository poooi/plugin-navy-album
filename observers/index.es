import { observe } from 'redux-observers'
import { store } from 'views/create-store'

import { pStateSaver } from './p-state-saver'
import { swfCacheSaver } from './swf-cache-saver'
import { subtitleLoader } from './subtitle-loader'
import { gameUpdateDetector } from './game-update-detector'

let unsubscribe = null

const globalSubscribe = () => {
  if (unsubscribe !== null) {
    console.warn('expecting "unsubscribe" to be null')
    if (typeof unsubscribe === 'function')
      unsubscribe()
    unsubscribe = null
  }

  unsubscribe = observe(store, [
    pStateSaver,
    swfCacheSaver,
    subtitleLoader,
    gameUpdateDetector,
  ])
}

const globalUnsubscribe = () => {
  if (typeof unsubscribe !== 'function') {
    console.warn(`unsubscribe is not a function`)
  } else {
    unsubscribe()
    unsubscribe = null
  }
}

export {
  globalSubscribe,
  globalUnsubscribe,
}
