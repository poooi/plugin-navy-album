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
  shipsMasterDataSelector,
  debuffInfoSelector,
  compareShipNaturalOrder,
} from '../../selectors'
import {
  isAbyssalShipMstId,
} from '../../game-misc'

const shipsAlbumSelector = createSelector(
  uiSelector,
  ui => ui.shipsAlbum)

const listOptionsSelector = createSelector(
  shipsAlbumSelector,
  sa => sa.listOptions)

const shipViewerSelector = createSelector(
  shipsAlbumSelector,
  sa => sa.shipViewer
)

const searchTextSelector = createSelector(
  shipsAlbumSelector,
  sa => sa.searchText
)

const mstIdSelector = createSelector(
  shipViewerSelector,
  sv => sv.mstId
)

const levelSelector = createSelector(
  shipViewerSelector,
  sv => sv.level
)

const filteredShipsInfoSelector = createSelector(
  searchTextSelector,
  shipsInfoSelector,
  (searchText, shipsInfo) => {
    if (searchText === '')
      return shipsInfo

    const searchTextLC = _.lowerCase(searchText)
    // convert raw data to lowercase string, then compare with searchText
    // to implement a case-insensitive search
    const matchFound = raw =>
      _.lowerCase(String(raw)).indexOf(searchTextLC) !== -1

    return shipsInfo.filter(si =>
      matchFound(si.name) ||
      matchFound(si.yomi) ||
      matchFound(si.romaji) ||
      matchFound(si.mstId)
    )
  }
)

// "stage1" takes into account friendly / abyssal options
const shipsInfoStage1Selector = createSelector(
  filteredShipsInfoSelector,
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
        s => !isAbyssalShipMstId(s.mstId) :
        // otherwise it must be the case where
        s => isAbyssalShipMstId(s.mstId)
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
        xs => xs.sort(compareShipNaturalOrder)

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

const hasDebuffedGraphsSelector = createSelector(
  mstIdSelector,
  debuffInfoSelector,
  (mstId, debuffInfo) =>
    (mstId in debuffInfo) && debuffInfo[mstId] === true
)

const shipMasterDataSelector = createSelector(
  mstIdSelector,
  shipsMasterDataSelector,
  (mstId, $ships) => $ships[mstId]
)

export {
  shipsAlbumSelector,
  listOptionsSelector,
  searchTextSelector,
  shipsInfoSelectorForView,
  shipViewerSelector,
  mstIdSelector,
  levelSelector,
  shipGraphSelector,
  shipMasterDataSelector,
  hasDebuffedGraphsSelector,
  debuffFlagSelector,
}
