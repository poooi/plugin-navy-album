import _ from 'lodash'
import { createSelector } from 'reselect'
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

export { remodelInfoSelector }
