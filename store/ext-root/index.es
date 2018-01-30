import { combineReducers } from 'redux'

import { reducer as ui } from './ui'
import { reducer as swfDatabase } from './swf-database'
import { reducer as subtitle } from './subtitle'
import { reducer as gameUpdate } from './game-update'
import { reducer as swfCache } from './swf-cache'

const reducer = combineReducers({
  ui, swfDatabase, subtitle, gameUpdate, swfCache,
})

const initState = reducer(undefined, {type: '@@INIT'})

export { reducer, initState }
