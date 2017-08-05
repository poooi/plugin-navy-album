import { observe } from 'redux-observers'
import { store } from 'views/create-store'

import { swfRequester } from './swf-requester'
import { subtitleLoader } from './subtitle-loader'
import { pStateSaver } from './p-state-saver'

const observeAll = () =>
  observe(store, [
    swfRequester,
    subtitleLoader,
    pStateSaver,
  ])

export { observeAll }
