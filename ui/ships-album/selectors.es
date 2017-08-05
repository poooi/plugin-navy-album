import _ from 'lodash'
import {
  projectorToComparator,
  flipComparator,
} from 'subtender'
import { createSelector } from 'reselect'
import { constSelector } from 'views/utils/selectors'

import {
  uiSelector,
  shipsInfoSelector,
  sortByRemodelFuncSelector,
  indexedShipGraphsSelector,
  swfDatabaseSelector,
} from '../../selectors'

const shipsAlbumSelector = createSelector(
  uiSelector,
  ui => ui.shipsAlbum)

const listOptionsSelector = createSelector(
  shipsAlbumSelector,
  sa => sa.listOptions)

const isAbyssal = s => s.mstId > 1500

// "stage1" takes into account friendly / abyssal options
const shipsInfoStage1Selector = createSelector(
  shipsInfoSelector,
  listOptionsSelector,
  (shipsInfo, {showSides}) => {
    const {friendly, abyssal} = showSides
    // since there are only 4 cases,
    // let's take our time to consider all of them
    if (friendly && abyssal)
      return shipsInfo
    if (!friendly && !abyssal)
      return []

    // either friendly === true or abyssal === true
    return shipsInfo.filter(
      friendly ?
        s => !isAbyssal(s) :
        // otherwise it must be the case where
        isAbyssal
    )
  }
)

/*

   "stage2" takes into account "groupShipTypes":

   - true: returns an Object whose keys are stypes and values Array of ShipInfo.
   - false: returns an Array of ShipInfo - since there's nothing to be groupped in this stage.

 */
const shipsInfoStage2Selector = createSelector(
  shipsInfoStage1Selector,
  listOptionsSelector,
  (shipsInfo, {groupShipTypes}) =>
    groupShipTypes ?
      _.groupBy(shipsInfo, 'stype') :
      shipsInfo
)

/*
   stage3 takes into account ship remodel info:

   - when groupRemodels is true, "sortByRemodelFunc" is used to sort the array
   - otherwise simply use mstId to sort

 */
const shipsInfoStage3Selector = createSelector(
  shipsInfoStage2Selector,
  listOptionsSelector,
  constSelector,
  sortByRemodelFuncSelector,
  (
    shipsInfoObjOrArr,
    {groupRemodels},
    {$shipTypes},
    sortByRemodelFunc,
  ) => {
    // in-place sort should be fine, nothing except this
    // selector would take stage2 results.
    const sortArray =
      groupRemodels ?
        sortByRemodelFunc :
        xs => xs.sort(projectorToComparator(s => s.mstId))

    /*
       unify two different structures in stage2 to get the
       "wrappedShipsInfo" (Array), whose elements are one of the following:

       - {type: 'ship', info: <ShipInfo>}
       - {type: 'stype', typeName: <string>, stype: <number>}

     */
    let groupped
    let wrappedShipsInfo
    if (Array.isArray(shipsInfoObjOrArr)) {
      groupped = false
      wrappedShipsInfo = sortArray(shipsInfoObjOrArr).map(s =>
        ({type: 'ship', info: s}))
    } else {
      groupped = true
      // Array of [ stype(number), sorted ShipInfo ]
      const sortedShipsInfoGroups =
        _.toPairs(shipsInfoObjOrArr).map(([stypeStr, stypeShipsInfo]) =>
          [
            // convert stype back to number for sorting
            Number(stypeStr),
            // convert shipsInfo (of the same stype)
            sortArray(stypeShipsInfo),
          ]
        ).sort(
          // ship type are sorted numerically in descending order
          // which is to be consistent with the order in game.
          flipComparator(projectorToComparator(([stype]) => stype))
        )
      wrappedShipsInfo =
        _.flatMap(
          sortedShipsInfoGroups, ([stype, stypeShipsInfo]) => {
            const stypeItem = {
              type: 'stype',
              typeName: $shipTypes[stype].api_name,
              stype,
            }
            return [
              stypeItem,
              ...stypeShipsInfo.map(s =>
                ({type: 'ship', info: s})),
            ]
          })
    }
    return {groupped, wrappedShipsInfo}
  }
)

const shipsInfoSelectorForView = shipsInfoStage3Selector

const shipViewerSelector = createSelector(
  shipsAlbumSelector,
  sa => sa.shipViewer
)

const mstIdSelector = createSelector(
  shipViewerSelector,
  sv => sv.mstId
)

const levelSelector = createSelector(
  shipViewerSelector,
  sv => sv.level
)

const shipGraphSelector = createSelector(
  indexedShipGraphsSelector,
  mstIdSelector,
  (indexedShipGraphs, mstId) =>
    _.get(indexedShipGraphs,mstId)
)

const debuffFlagSelector = createSelector(
  shipViewerSelector,
  sv => sv.debuffFlag
)

const shipGraphSourcesSelector = createSelector(
  mstIdSelector,
  swfDatabaseSelector,
  debuffFlagSelector,
  (mstId, swfDatabase, debuffFlag) =>
    _.get(
      swfDatabase,
      [
        'shipDb',
        mstId,
        (mstId > 1500 && debuffFlag) ?
          'imagesDebuffed' :
          'images',
      ]) || {}
)

const hasDebuffedGraphsSelector = createSelector(
  mstIdSelector,
  swfDatabaseSelector,
  (mstId, swfDatabase) =>
    mstId > 1500 &&
    ! _.isEmpty(
      _.get(swfDatabase,['shipDb',mstId,'imagesDebuffed'])
    )
)

const shipMasterDataSelector = createSelector(
  mstIdSelector,
  constSelector,
  (mstId, {$ships}) => $ships[mstId]
)

export {
  shipsAlbumSelector,
  listOptionsSelector,
  shipsInfoSelectorForView,
  shipViewerSelector,
  mstIdSelector,
  levelSelector,
  shipGraphSelector,
  shipGraphSourcesSelector,
  shipMasterDataSelector,
  hasDebuffedGraphsSelector,
  debuffFlagSelector,
}
