import _ from 'lodash'
import { createSelector } from 'reselect'
import {
  generalComparator,
} from 'subtender'
import { constSelector } from 'views/utils/selectors'
import { indexedShipGraphsSelector } from './common'

const digestConstObj = x =>
  _.keys(x).map(Number).sort(generalComparator)

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
      /*
         note: ship graph version is just the `[0]` part of api_version,
         which can be confirmed from ShipGraphModel.version of main.js
       */
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
