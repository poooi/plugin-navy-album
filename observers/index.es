import { observe } from 'redux-observers'
import { store } from 'views/create-store'

import { pStateSaver } from './p-state-saver'
import { subtitleLoader } from './subtitle-loader'
import {
  swfCacheUpdater,
  swfCacheIndexFileUpdater,
} from './swf-cache-updater'
import { gameUpdateDetector } from './game-update-detector'

const observeAll = () =>
  observe(store, [
    pStateSaver,
    subtitleLoader,

    swfCacheUpdater,
    swfCacheIndexFileUpdater,

    gameUpdateDetector,
  ])

export { observeAll }
