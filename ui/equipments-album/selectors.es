import _ from 'lodash'
import { createSelector } from 'reselect'
import { constSelector } from 'views/utils/selectors'
import { projectorToComparator } from 'subtender'

import {
  uiSelector,
} from '../../selectors'
import {
  isAbyssalEquipMstId,
} from '../../game-misc'

const equipmentsAlbumSelector = createSelector(
  uiSelector,
  ui => ui.equipmentsAlbum)

const listOptionsSelector = createSelector(
  equipmentsAlbumSelector,
  ea => ea.listOptions)

const searchTextSelector = createSelector(
  equipmentsAlbumSelector,
  ea => ea.searchText
)

// order not specified
const allEquipsRawSelector = createSelector(
  constSelector,
  ({$equips}) => _.values($equips)
)

const isAbyssalEquip = s => isAbyssalEquipMstId(s.api_id)

const filteredEquipsRawSelector = createSelector(
  searchTextSelector,
  allEquipsRawSelector,
  (searchText, allEquipsRaw) =>
    searchText === '' ?
      allEquipsRaw :
      allEquipsRaw.filter(raw =>
        raw.api_name.indexOf(searchText) !== -1 ||
        String(raw.api_id).indexOf(searchText) !== -1
      )
)

// "stage1" takes into account sides.
const equipsRawStage1Selector = createSelector(
  filteredEquipsRawSelector,
  listOptionsSelector,
  (equipsRaw, {showSides}) => {
    const {friendly, abyssal} = showSides
    if (friendly && abyssal)
      return equipsRaw
    if (!friendly && !abyssal)
      return []
    return equipsRaw.filter(
      friendly ?
        s => !isAbyssalEquip(s) :
        // otherwise it must be the case where
        isAbyssalEquip
    )
  }
)

/*

   "stage2" takes into account "groupEquipTypes":

   - true: returns an Object whose keys are stypes and values Array of ShipInfo.
   - false: returns an Array of ShipInfo - since there's nothing to be groupped in this stage.

 */
const equipsRawStage2Selector = createSelector(
  equipsRawStage1Selector,
  listOptionsSelector,
  (equipsRaw, {groupEquipTypes}) =>
    groupEquipTypes ?
      _.groupBy(equipsRaw, e => e.api_type[2]) :
      equipsRaw
)

const equipsRawSelectorForView = createSelector(
  equipsRawStage2Selector,
  constSelector,
  (equipsRawObjOrArr, {$equipTypes}) => {
    const sortArray =
      xs => xs.sort(projectorToComparator(x => x.api_id))
    const trRaw = raw => ({
      type: 'equip',
      mstId: raw.api_id,
      icon: raw.api_type[3],
      name: raw.api_name,
    })
    let groupped
    let wrappedEquipsRaw
    if (Array.isArray(equipsRawObjOrArr)) {
      groupped = false
      wrappedEquipsRaw = sortArray(equipsRawObjOrArr).map(trRaw)
    } else {
      groupped = true
      const sortedEquipsRawGroups =
        _.toPairs(equipsRawObjOrArr).map(([eqTypeStr, etypeRaws]) =>
          [
            Number(eqTypeStr),
            sortArray(etypeRaws),
          ]
        ).sort(projectorToComparator(([eqType]) => eqType))
      wrappedEquipsRaw =
        _.flatMap(
          sortedEquipsRawGroups, ([eqType, etypeRaws]) => {
            const etypeItem = {
              type: 'etype',
              typeName: $equipTypes[eqType].api_name,
              etype: eqType,
            }
            return [
              etypeItem,
              ...etypeRaws.map(trRaw),
            ]
          })
    }
    return {groupped, wrappedEquipsRaw}
  }
)


const equipViewerSelector = createSelector(
  equipmentsAlbumSelector,
  ea => ea.equipViewer
)


const mstIdSelector = createSelector(
  equipViewerSelector,
  ev => ev.mstId
)

const equipRawInfoSelector = createSelector(
  mstIdSelector,
  constSelector,
  (mstId, {$equips = {}, $equipTypes = {}}) => {
    if (_.isEmpty($equips) || _.isEmpty($equipTypes))
      return {$equip: null, $equipType: null}
    const $equip = $equips[mstId]
    if (_.isEmpty($equip))
      return {$equip: null, $equipType: null}
    const etype = $equip.api_type[2]
    const $equipType = $equipTypes[etype]
    if (_.isEmpty($equipType))
      return {$equip: null, $equipType: null}

    return {$equip, $equipType}
  }
)

export {
  isAbyssalEquipMstId,
  listOptionsSelector,
  equipsRawSelectorForView,
  equipViewerSelector,
  mstIdSelector,
  equipRawInfoSelector,
  searchTextSelector,
}
