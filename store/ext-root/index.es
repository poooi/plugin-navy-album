import { combineReducers } from 'redux'

import { reducer as ui } from './ui'
import { reducer as swfDatabase } from './swf-database'
import { reducer as subtitle } from './subtitle'
import { reducer as gameUpdate } from './game-update'

const reducer = combineReducers({
  ui, swfDatabase, subtitle, gameUpdate,
})

const initState = reducer(undefined, {type: '@@INIT'})

export { reducer, initState }
