import _ from 'lodash'
import { createStructuredSelector } from 'reselect'
import { observer } from 'redux-observers'

import {
  constDigestSelector,
  gameUpdateSelector,
} from '../selectors'
import {
  withBoundActionCreator,
} from '../store'
import {
  summarizeChanges,
} from '../game-misc'

const gameUpdateDetector = observer(
  createStructuredSelector({
    constDigest: constDigestSelector,
    gameUpdate: gameUpdateSelector,
  }),
  (dispatch, cur, _prev) => {
    if (!cur.gameUpdate.ready)
      return
    if (cur.gameUpdate.digest === null) {
      console.error(
        `invariant violation: gameUpdate.digest is null when it's ready`
      )
      return
    }
    if (!_.isEqual(cur.constDigest,cur.gameUpdate.digest)) {
      // game update detected
      const newSummary = summarizeChanges(cur.constDigest,cur.gameUpdate.digest)
      if (newSummary !== null) {
        // modify {digest, summary} of the store
        withBoundActionCreator(
          ({gameUpdateNewDigest,gameUpdateNewSummary}) => {
            gameUpdateNewDigest(cur.constDigest)
            gameUpdateNewSummary(newSummary)
          },
          dispatch
        )
      }
    }
  },
  /*
     just in case that extStore.gameUpdate is ready ahead of
     observer initialization
   */
  {skipInitialCall: false}
)

export {
  summarizeChanges,
  gameUpdateDetector,
}
