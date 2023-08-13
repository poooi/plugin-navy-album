import _ from 'lodash'
import { createSelector } from 'reselect'

import {
  gameUpdateSelector,
  isMasterIdSpecialCGFuncSelector,
  sortByRemodelFuncSelector,
} from '../../selectors'
import {
  isAbyssalShipMstId,
  isAbyssalEquipMstId,
} from '../../game-misc'

const genSummary = () => {
  const {getStore} = window
  const {$ships, $equips} = getStore('const')
  const allShipMstIds = _.values($ships).map(x => x.api_id)
  const allEquipMstIds = _.values($equips).map(x => x.api_id)
  const genShips = () => allShipMstIds.filter(() => Math.random() < 0.02 )
  const genEquips = () => allEquipMstIds.filter(() => Math.random() < 0.02 )
  return {
    addedShipMstIds: genShips(),
    addedEquipMstIds: genEquips(),
    changedShipMstIds: [800, 801, 802, 855, 880], // genShips(),
  }
}

const fakedSummary = false && genSummary()

const summarySelector =
  fakedSummary
    ? (() => fakedSummary)
    : createSelector(
      gameUpdateSelector,
      gu => _.get(gu, 'summary') || {}
    )

const mkSimpleArrSelector = propName => createSelector(
  summarySelector,
  gu => _.get(gu, propName) || []
)

const addedEquipMstIdsSelector =
  mkSimpleArrSelector('addedEquipMstIds')
const addedShipMstIdsSelector =
  mkSimpleArrSelector('addedShipMstIds')
const changedShipMstIdsSelector =
  mkSimpleArrSelector('changedShipMstIds')

/*

   returns a function: mstId => 'abyssal' | 'special' | 'friendly'

   'abyssal': the mstId is for an abyssal ship
   'special': mstId of special CGs (could be a mstId in $shipGraph but not in $ships)
   'friendly': this mstId is for a friendly ship

 */
const mstIdToCategoryFuncSelector = createSelector(
  isMasterIdSpecialCGFuncSelector,
  isMstIdSpecialCG => _.memoize(mstId =>
    /* eslint-disable indent */
    isMstIdSpecialCG(mstId) ? 'special' :
      isAbyssalShipMstId(mstId) ? 'abyssal' : 'friendly'
    /* eslint-enable indent */
  )
)

/*
   reorganize summary into following structure:
   (all <Array>s are Arrays of mstId)

   {
     addedEquipMstIds: { friendly: <Array>, abyssal: <Array> },
     addedShipMstIds: { friendly: <Array>, abyssal: <Array> },
     changedShipMstIds: { friendly: <Array>, abyssal: <Array>, special: <Array> },
   }

 */
const reorganizedSummarySelector = createSelector(
  addedEquipMstIdsSelector,
  addedShipMstIdsSelector,
  changedShipMstIdsSelector,
  mstIdToCategoryFuncSelector,
  sortByRemodelFuncSelector,
  (
    addedEquipMstIds,
    addedShipMstIds,
    changedShipMstIds,
    mstIdToCat,
    sortByRemodel,
  ) => {
    const sortMstIds = xs =>
      sortByRemodel(xs.map(mstId => ({mstId}))).map(x => x.mstId)

    let newAddedEquipMstIds
    {
      const [abyssal, friendly] =
        _.partition(addedEquipMstIds,isAbyssalEquipMstId)
      newAddedEquipMstIds = {friendly, abyssal}
    }
    let newAddedShipMstIds
    {
      const {
        special: special=[],
        friendly: friendly=[],
        abyssal: abyssal=[],
      } = _.groupBy(addedShipMstIds, mstIdToCat)
      newAddedShipMstIds = {
        special,
        friendly: sortMstIds(friendly),
        abyssal,
      }
    }
    let newChangedShipMstIds
    {
      const {
        special: special=[],
        friendly: friendly=[],
        abyssal: abyssal=[],
      } = _.groupBy(changedShipMstIds, mstIdToCat)
      newChangedShipMstIds = {
        special,
        friendly: sortMstIds(friendly),
        abyssal,
      }
    }
    return {
      addedEquipMstIds: newAddedEquipMstIds,
      addedShipMstIds: newAddedShipMstIds,
      changedShipMstIds: newChangedShipMstIds,
    }
  }
)

export {
  reorganizedSummarySelector,
  mstIdToCategoryFuncSelector,
}
