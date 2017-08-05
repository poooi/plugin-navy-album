import { observe } from 'redux-observers'
import { store } from 'views/create-store'

import { swfRequester } from './swf-requester'
import { subtitleLoader } from './subtitle-loader'

const observeAll = () =>
  observe(store, [
    swfRequester,
    subtitleLoader,
  ])

export { observeAll }
