import _ from 'lodash'
import { createSelector } from 'reselect'
import { generalComparator } from 'subtender'
import {
  constSelector,
  extensionSelectorFactory,
  configSelector as poiConfigSelector,
  stateSelector as poiStateSelector,
  wctfSelector,
} from 'views/utils/selectors'
import { toRomaji } from 'wanakana'
import { initState } from '../store'
import {
  isAbyssalShipMstId,
  getShipImgPathHelper,
} from '../game-misc'

const extSelector = createSelector(
  extensionSelectorFactory('poi-plugin-navy-album'),
  ext => _.isEmpty(ext) ? initState : ext)

const mkExtPropSelector = _.memoize(propName =>
  createSelector(extSelector, ext => ext[propName]))

const uiSelector =
  mkExtPropSelector('ui')
const subtitleSelector =
  mkExtPropSelector('subtitle')
const gameUpdateSelector =
  mkExtPropSelector('gameUpdate')
const masterSelector =
  mkExtPropSelector('master')
const debuffInfoSelector =
  mkExtPropSelector('debuffInfo')

const indexedShipGraphsSelector = createSelector(
  constSelector,
  ({$shipgraph = {}}) => _.keyBy($shipgraph, 'api_id')
)

// all master ids including those for special CGs
const allMasterIdsSelector = createSelector(
  constSelector,
  ({$shipgraph=[], $ships={}}) => {
    const mstIds1 = $shipgraph.map(x => x.api_id)
    const mstIds2 = _.values($ships).map(x => x.api_id)
    return _.uniq([...mstIds1, ...mstIds2]).sort(generalComparator)
  }
)

const shipGraphInfoSelector = createSelector(
  constSelector,
  indexedShipGraphsSelector,
  allMasterIdsSelector,
  ({$ships}, indexedShipGraphs, allMstIds) =>
    allMstIds.map(mstId => {
      const $ship = $ships[mstId] || {api_name: ''}
      const $shipgraph = indexedShipGraphs[mstId]
      const shipName = $ship.api_name
      return {
        mstId,
        shipName,
        graphInfo: {
          fileName: $shipgraph.api_filename,
          versionStr: $shipgraph.api_version[0],
        },
      }
    })
)

const indexedShipGraphInfoSelector = createSelector(
  shipGraphInfoSelector,
  shipGraphInfo => _.keyBy(shipGraphInfo, 'mstId')
)

// returns an Array of ShipInfo, order is unspecified.
const shipsInfoSelector = createSelector(
  constSelector,
  ({$ships}) => _.values($ships).map($ship => {
    const mstId = $ship.api_id
    const sortNo = $ship.api_sort_id
    const name = $ship.api_name
    const stype = $ship.api_stype
    const yomi = $ship.api_yomi
    const romaji = sortNo ? toRomaji(yomi) : toRomaji(name)
    return {
      mstId,
      name,
      sortNo,
      stype,
      yomi,
      romaji,
    }
  })
)

const themeSelector = createSelector(
  poiConfigSelector,
  pc => _.get(pc, ['poi', 'appearance', 'theme'], 'dark'))

const infoSelector = createSelector(
  poiStateSelector,
  state => state.info
)

const serverIpSelector = createSelector(
  infoSelector,
  info => _.get(info,'server.ip')
)

const shipsMasterDataSelector = createSelector(
  constSelector,
  ({$ships}) => $ships
)

/*
   returns a function that tests whether a given ship master id is
   for special CG.
 */
const isMasterIdSpecialCGFuncSelector = createSelector(
  shipsMasterDataSelector,
  $ships => mstId => {
    // const.$ships not ready
    if (!$ships || typeof $ships !== 'object')
      return false

    if (isAbyssalShipMstId(mstId))
      return false
    const $ship = $ships[mstId]
    return (!$ship || !('api_sortno' in $ship))
  }
)

const wctfShipsSelector = createSelector(
  wctfSelector,
  w => _.get(w, 'ships', {})
)

/*
   provides two sets of all valid master ids:

   - for ships this means all ids from $ships and those ids held by special CGs
   - for equips this means all ids from $equips

 */
const validMasterIdSetsSelector = createSelector(
  constSelector,
  ({$ships, $shipgraph, $equips}) => {
    const shipMstIds = _.values($ships).map(x => x.api_id)
    const shipCGMstIds = _.values($shipgraph).map(x => x.api_id)
    const equipMstIds = _.values($equips)
    const shipIdSet = new Set([...shipMstIds, ...shipCGMstIds])
    const equipIdSet = new Set(equipMstIds)
    return {
      shipIdSet,
      equipIdSet,
    }
  }
)

const poiVolumeSelector = createSelector(
  poiConfigSelector,
  c => _.get(c,'poi.notify.volume',0.8)
)

const rawShipGraphArrSelector = createSelector(
  constSelector,
  s => _.get(s, ['$shipgraph'], [])
)

const getShipImgPathFuncSelector = createSelector(
  rawShipGraphArrSelector,
  sgRaw => getShipImgPathHelper(sgRaw)
)

/*
  Selects a function ready for `src` attribute of an `img` HTML tag.
 */
const getShipImgSrcFuncSelector = createSelector(
  serverIpSelector,
  getShipImgPathFuncSelector,
  (serverIp, getShipImgPath) => (...args) => {
    // TODO: hacky
    const p = getShipImgPath(...args)
    return _.startsWith(p, 'file:') ? p : `http://${serverIp}${p}`
  }
)

export {
  extSelector,
  uiSelector,
  shipGraphInfoSelector,
  shipsInfoSelector,
  indexedShipGraphsSelector,
  indexedShipGraphInfoSelector,
  themeSelector,
  serverIpSelector,
  poiConfigSelector,
  subtitleSelector,
  gameUpdateSelector,
  shipsMasterDataSelector,
  isMasterIdSpecialCGFuncSelector,
  wctfShipsSelector,
  validMasterIdSetsSelector,
  masterSelector,
  poiVolumeSelector,
  debuffInfoSelector,
  getShipImgSrcFuncSelector,
}

