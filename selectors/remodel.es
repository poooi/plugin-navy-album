import _ from 'lodash'
import { createSelector } from 'reselect'
import {
  projectorToComparator,
  generalComparator,
} from 'subtender'
import { constSelector } from 'views/utils/selectors'
import { shipRemodelInfoSelector as remodelInfoSelector } from 'subtender/poi'

import { isAbyssalShipMstId } from '../game-misc'

/*
  Compares ShipInfo utilizing both sortNo and mstId:

  - friendly ships are always in front of abyssals
  - by using sortNo, ships from same class tends to be organized closer.

 */
const compareShipNaturalOrder = (l, r) => {
  const absL = isAbyssalShipMstId(l.mstId)
  const absR = isAbyssalShipMstId(r.mstId)

  if (absL && absR) {
    return generalComparator(l.mstId, r.mstId)
  }

  if (!absL && absR) {
    return -1
  }

  if (absL && !absR) {
    return 1
  }

  return generalComparator(l.sortNo, r.sortNo)
}

/*
   returns a function:

   sortByRemodelFunc: Array<{mstId, ...}> => Array<{mstId, ...}>

   to sort by remodeling is to

   (1) group ships by their originalMstIds
     (we pretend that abyssals has a originalMstId of "abyssal")

   (2) rearrange ships in every group following the remodel chain

   (3) sort groups by originalMstId (mapping "abyssal" to +Infinity)

   (4) flatten

 */
const sortByRemodelFuncSelector = createSelector(
  remodelInfoSelector,
  constSelector,
  ({remodelChains, originMstIdOf},{$ships}) =>
    shipsInfo => {
      const mstIdToOrigin = mstId =>
        isAbyssalShipMstId(mstId) ?
          'abyssal' :
          (
            originMstIdOf[mstId] ||
            // shouldn't happen, but just in case.
            (
              console.warn(`originMstId not found for ${mstId}`),
              mstId
            )
          )

      // step 1
      const grouppedShipsInfo =
        _.groupBy(shipsInfo, s => mstIdToOrigin(s.mstId))

      // step 2
      const mstIdComparator = projectorToComparator(s => s.mstId)
      const sortedGroupPairs =
        _.toPairs(grouppedShipsInfo).map(([originMstIdStr, inputGroup]) => {
          if (originMstIdStr === 'abyssal') {
            return {
              key: {mstId: +Infinity, sortNo: undefined},
              // sort group inplace for abyssal ships
              group: inputGroup.sort(mstIdComparator),
            }
          } else {
            const origMstId = Number(originMstIdStr)
            const sortNo = _.get($ships, [origMstId, 'api_sort_id'])
            const key = {mstId: origMstId, sortNo}
            const remodelChain = remodelChains[origMstId] || []
            return {
              key,
              group: _.compact(
                remodelChain.map(mstId =>
                  inputGroup.find(s => s.mstId === mstId))
              ),
            }
          }
          // unreachable
        }).sort(
          // step 3
          (l, r) => compareShipNaturalOrder(l.key, r.key)
        )
      return _.flatMap(sortedGroupPairs, ({group}) => group)
    }
)

export {
  remodelInfoSelector,
  sortByRemodelFuncSelector,
  compareShipNaturalOrder,
}
