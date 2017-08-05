import _ from 'lodash'
import shallowEqual from 'shallowequal'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import { observer } from 'redux-observers'

import { stateToPState, savePState } from '../p-state'
import { extSelector, uiSelector } from '../selectors'

const pStateSelector = createSelector(
  extSelector,
  ext => stateToPState(ext)
)

// to tell whether we are in the middle of some other loading process
// observer saves only when all observering parts (for now only ui) are ready
const readySelector = createSelector(
  uiSelector,
  ui => ui.ready
)

const debouncedSavePState = _.debounce(
  pState => setTimeout(() => savePState(pState)),
  500
)

const pStateSaver = observer(
  createStructuredSelector({
    ready: readySelector,
    pState: pStateSelector,
  }),
  (_dispatch, cur, prev) => {
    if (!cur.ready)
      return
    if (!shallowEqual(cur.pState, prev.pState)) {
      debouncedSavePState(cur.pState)
    }
  }
)

export { pStateSaver }
