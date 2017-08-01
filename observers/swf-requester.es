import { observer } from 'redux-observers'
import { shipGraphPathSelector } from '../ui/ships-album/selectors'
import { asyncBoundActionCreator } from '../store'

const swfRequester = observer(
  shipGraphPathSelector,
  (dispatch, cur, prev) => {
    if (
      cur && (
        typeof prev === 'undefined' ||
        cur !== prev
      )
    ) {
      asyncBoundActionCreator(
        ({requestSwf}) => requestSwf(cur),
        dispatch
      )
    }
  },
  {skipInitialCall: false}
)

export { swfRequester }
