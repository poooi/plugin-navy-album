import { combineReducers } from 'redux'

import { reducer as ui } from './ui'

const reducer = combineReducers({ui})

export { reducer }
