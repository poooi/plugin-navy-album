import { observe } from 'redux-observers'
import { store } from 'views/create-store'

import { pStateSaver } from './p-state-saver'
import { subtitleLoader } from './subtitle-loader'
import { shipGraphRequester } from './ship-graph-requester'
import {
  swfCacheUpdater,
  swfCacheIndexFileUpdater,
} from './swf-cache-updater'

const observeAll = () =>
  observe(store, [
    pStateSaver,
    subtitleLoader,
    shipGraphRequester,

    swfCacheUpdater,
    swfCacheIndexFileUpdater,
  ])

export { observeAll }
