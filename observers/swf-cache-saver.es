import _ from 'lodash'
import {
  createSelector,
  createStructuredSelector,
} from 'reselect'
import { observer } from 'redux-observers'

import {
  swfCacheSelector,
} from '../selectors'
import { saveSwfCache } from '../swf-cache'

const mkSelector = propName => createSelector(
  swfCacheSelector,
  sc => sc[propName]
)

const readySelector = mkSelector('ready')
const shipSelector = mkSelector('ship')

const pSwfCacheSelector = createSelector(
  shipSelector,
  ship => ({ship})
)

const debouncedSaveSwfCache = _.debounce(
  pSwfCache => setTimeout(() => saveSwfCache(pSwfCache)),
  500
)

const swfCacheSaver = observer(
  createStructuredSelector({
    ready: readySelector,
    pSwfCache: pSwfCacheSelector,
  }),
  (_dispatch, cur, prev) => {
    if (!cur.ready || !prev.ready)
      return
    if (cur.pSwfCache !== prev.pSwfCache) {
      debouncedSaveSwfCache(cur.pSwfCache)
    }
  }
)

export { swfCacheSaver }
