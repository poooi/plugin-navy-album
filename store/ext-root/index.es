import { combineReducers } from 'redux'

import { reducer as ui } from './ui'
import { reducer as subtitle } from './subtitle'
import { reducer as gameUpdate } from './game-update'
import { reducer as swfCache } from './swf-cache'
import { reducer as debuffInfo } from './debuff-info'
import { reducer as master } from './master'

const reducer = combineReducers({
  ui, subtitle, gameUpdate, swfCache, master, debuffInfo,
})

const initState = reducer(undefined, {type: '@@INIT'})

export { reducer, initState }
