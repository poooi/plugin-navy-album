import _ from 'lodash'
import { createSelector } from 'reselect'
import { projectorToComparator } from 'subtender'
import { constSelector } from 'views/utils/selectors'

const remodelInfoSelector = createSelector(
  constSelector,
  ({$ships}) => {
    // non-abyssal ships
    const mstIds = Object.keys($ships).map(Number).filter(k => k < 1501)
    // set of masterIds that has some other ship pointing to it (through remodel)
    const afterMstIdSet = new Set()

    mstIds.map(mstId => {
      const $ship = $ships[mstId]
      const afterMstId = Number($ship.api_aftershipid)
      if (afterMstId !== 0)
        afterMstIdSet.add(afterMstId)
    })

    // all those that has nothing pointing to them are originals
    const originMstIds = mstIds.filter(mstId => !afterMstIdSet.has(mstId))

    /*
       remodelChains[originMstId] = <RemodelChain>

       - originMstId: master id of the original ship
       - RemodelChain: an Array of master ids, sorted by remodeling order.
     */
    const remodelChains = _.fromPairs(originMstIds.map(originMstId => {
      const searchRemodels = (mstId, results=[]) => {
        if (results.includes(mstId))
          return results

        const newResults = [...results, mstId]
        const $ship = $ships[mstId]
        const afterMstId = Number($ship.api_aftershipid)
        if (afterMstId !== 0) {
          return searchRemodels(afterMstId,newResults)
        } else {
          return newResults
        }
      }
      return [originMstId, searchRemodels(originMstId)]
    }))

    // originMstIdOf[<master id>] = <original master id>
    const originMstIdOf = {}
    Object.entries(remodelChains).map(([originMstId, remodelChain]) => {
      remodelChain.map(mstId => {
        originMstIdOf[mstId] = originMstId
      })
    })
    return {remodelChains, originMstIdOf}
  }
)

/*
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
            const remodelChain = remodelChains[originalMstId]

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
