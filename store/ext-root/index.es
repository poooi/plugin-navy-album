import { combineReducers } from 'redux'

import { reducer as ui } from './ui'
import { reducer as swfDatabase } from './swf-database'

const reducer = combineReducers({ui, swfDatabase})

export { reducer }
