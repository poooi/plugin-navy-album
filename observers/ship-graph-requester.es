import { observer } from 'redux-observers'
import { mstIdSelector } from '../ui/ships-album/selectors'
import { asyncBoundActionCreator } from '../store'

const shipGraphRequester = observer(
  mstIdSelector,
  (dispatch, cur, prev) => {
    if (
      cur && (
        typeof prev === 'undefined' ||
        cur !== prev
      )
    ) {
      asyncBoundActionCreator(
        ({requestShipGraph}) => requestShipGraph(cur),
        dispatch
      )
    }
  },
  {skipInitialCall: false}
)

export { shipGraphRequester }
