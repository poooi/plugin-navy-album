import { combineReducers } from 'redux'

import { reducer as ui } from './ui'
import { reducer as swfDatabase } from './swf-database'
import { reducer as subtitle } from './subtitle'

const reducer = combineReducers({ui, swfDatabase, subtitle})

export { reducer }
