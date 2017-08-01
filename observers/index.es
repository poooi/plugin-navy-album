import { observe } from 'redux-observers'
import { store } from 'views/create-store'

import { swfRequester } from './swf-requester'

const observeAll = () =>
  observe(store, [
    swfRequester,
  ])

export { observeAll }
