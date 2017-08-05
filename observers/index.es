import { observe } from 'redux-observers'
import { store } from 'views/create-store'

import { shipGraphRequester } from './ship-graph-requester'
import { subtitleLoader } from './subtitle-loader'
import { pStateSaver } from './p-state-saver'

const observeAll = () =>
  observe(store, [
    shipGraphRequester,
    subtitleLoader,
    pStateSaver,
  ])

export { observeAll }
