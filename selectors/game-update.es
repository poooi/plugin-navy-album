import { createSelector } from 'reselect'
import {
  generalComparator,
} from 'subtender'
import { constSelector } from 'views/utils/selectors'
import { indexedShipGraphsSelector } from './common'

const digestConstObj = x =>
  Object.keys(x).map(Number).sort(generalComparator)

/*
   generates a digest of store.const:

   - shipDigests: Array of [ship masterId, graphDigest]
   - equipMstIds: Array of equipment masterIds

   all sorted by ids

 */
const constDigestSelector = createSelector(
  constSelector,
  indexedShipGraphsSelector,
  ({$ships,$equips},indexedShipGraphs) => {
    const equipMstIds = digestConstObj($equips)
    const shipDigests = digestConstObj($ships).map(mstId => {
      const $shipGraph = indexedShipGraphs[mstId]
      return [
        mstId,
        `${$shipGraph.api_filename}#${$shipGraph.api_version[0]}`,
      ]
    })

    return {
      equipMstIds,
      shipDigests,
    }
  }
)

export { constDigestSelector }
