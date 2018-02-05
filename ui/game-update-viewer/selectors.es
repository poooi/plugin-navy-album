import _ from 'lodash'
import { createSelector } from 'reselect'
import { selectorTester } from 'subtender/poi'

import {
  gameUpdateSelector,
  isMasterIdSpecialCGFuncSelector,
} from '../../selectors'

const summarySelector = createSelector(
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

const isAbyssalMstId = mstId => mstId > 1500
const isAbyssalEquipMstId = eMstId => eMstId > 500

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
      isAbyssalMstId(mstId) ? 'abyssal' : 'friendly'
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
  (addedEquipMstIds, addedShipMstIds, changedShipMstIds, mstIdToCat) => {
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
      newAddedShipMstIds = {special, friendly, abyssal}
    }
    let newChangedShipMstIds
    {
      const {
        special: special=[],
        friendly: friendly=[],
        abyssal: abyssal=[],
      } = _.groupBy(changedShipMstIds, mstIdToCat)
      newChangedShipMstIds = {special, friendly, abyssal}
    }
    return {
      addedEquipMstIds: newAddedEquipMstIds,
      addedShipMstIds: newAddedShipMstIds,
      changedShipMstIds: newChangedShipMstIds,
    }
  }
)

export { reorganizedSummarySelector }
