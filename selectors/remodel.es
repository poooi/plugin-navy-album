import _ from 'lodash'
import { createSelector } from 'reselect'
import { projectorToComparator } from 'subtender'
import { shipRemodelInfoSelector as remodelInfoSelector } from 'subtender/poi'

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
  ({remodelChains, originMstIdOf}) =>
    shipsInfo => {
      const mstIdToOrigin = mstId =>
        mstId > 1500 ?
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
          let key
          let group
          if (originMstIdStr === 'abyssal') {
            key = +Infinity
            // sort group inplace.
            group = inputGroup.sort(mstIdComparator)
          } else {
            key = Number(originMstIdStr)
            const originalMstId = key
            // TODO: figure out how exactly does this happen.
            const remodelChain = remodelChains[originalMstId] || []

            group = _.compact(
              remodelChain.map(mstId =>
                inputGroup.find(s => s.mstId === mstId))
            )
          }
          return {key, group}
        }).sort(
          // step 3
          projectorToComparator(({key}) => key)
        )
      return _.flatMap(sortedGroupPairs, ({group}) => group)
    }
)

export {
  remodelInfoSelector,
  sortByRemodelFuncSelector,
}
