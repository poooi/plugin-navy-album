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
import { getShipFilePath } from '../swf-cache'

const extSelector = createSelector(
  extensionSelectorFactory('poi-plugin-navy-album'),
  ext => _.isEmpty(ext) ? [] : ext)

const mkExtPropSelector = _.memoize(propName =>
  createSelector(extSelector, ext => ext[propName]))

const uiSelector =
  mkExtPropSelector('ui')
const swfDatabaseSelector =
  mkExtPropSelector('swfDatabase')
const swfCacheSelector =
  mkExtPropSelector('swfCache')
const subtitleSelector =
  mkExtPropSelector('subtitle')
const gameUpdateSelector =
  mkExtPropSelector('gameUpdate')

const indexedShipGraphsSelector = createSelector(
  constSelector,
  ({$shipgraph}) => _.keyBy($shipgraph, 'api_id'))

const shipGraphInfoSelector = createSelector(
  constSelector,
  indexedShipGraphsSelector,
  ({$ships}, indexedShipGraphs) => {
    const mstIds = Object.keys($ships)
      .map(Number).sort(generalComparator)
    return mstIds.map(mstId => {
      const $ship = $ships[mstId]
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
  }
)

const indexedShipGraphInfoSelector = createSelector(
  shipGraphInfoSelector,
  shipGraphInfo => _.keyBy(shipGraphInfo, 'mstId')
)

// returns an Array of ShipInfo, order is unspecified.
const shipsInfoSelector = createSelector(
  constSelector,
  ({$ships}) => Object.values($ships).map($ship => {
    const mstId = $ship.api_id
    const sortNo = $ship.api_sortno
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
  pc => _.get(pc, 'poi.theme', 'paperdark'))

const infoSelector = createSelector(
  poiStateSelector,
  state => state.info
)

const serverIpSelector = createSelector(
  () => window.serverIp,
  infoSelector,
  (wServerIp, info) => {
    const rServerIp = _.get(info,'server.ip')
    return rServerIp || wServerIp
  }
)

const shipGraphSourceFuncSelector = createSelector(
  swfCacheSelector,
  swfCache =>
    (mstId, characterId, debuffFlag=false) => {
      const mstIdX = debuffFlag ? `${mstId}_d` : String(mstId)
      const fileName = _.get(swfCache, ['ship', mstIdX, 'files', characterId])
      if (fileName) {
        const path = getShipFilePath(mstIdX)(fileName)
        return path
      } else {
        return ''
      }
    }
)

const shipsMasterDataSelector = createSelector(
  constSelector,
  ({$ships}) => $ships
)

const isMasterIdAbyssalShip = mstId => mstId > 1500

/*
   returns a function that tests whether a given ship master id is
   for special CG.
 */
const isMasterIdSpecialCGFuncSelector = createSelector(
  shipsMasterDataSelector,
  $ships => mstId => {
    if (!$ships || typeof $ships !== 'object')
      return false
    if (isMasterIdAbyssalShip(mstId))
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
  ({$ships, $shipgraph: _ignored, $equips}) => {
    const shipMstIds = _.values($ships).map(x => x.api_id)
    // const shipCGMstIds = _.values($shipgraph).map(x => x.api_id)
    // we are not ready to handle special CGs, leaving this blank for now.
    const shipCGMstIds = []
    const equipMstIds = _.values($equips)
    const shipIdSet = new Set([...shipMstIds, ...shipCGMstIds])
    const equipIdSet = new Set(equipMstIds)
    return {
      shipIdSet,
      equipIdSet,
    }
  }
)

export {
  extSelector,
  uiSelector,
  shipGraphInfoSelector,
  shipsInfoSelector,
  swfDatabaseSelector,
  swfCacheSelector,
  indexedShipGraphsSelector,
  indexedShipGraphInfoSelector,
  themeSelector,
  serverIpSelector,
  poiConfigSelector,
  subtitleSelector,
  gameUpdateSelector,
  shipGraphSourceFuncSelector,
  shipsMasterDataSelector,
  isMasterIdSpecialCGFuncSelector,
  wctfShipsSelector,
  validMasterIdSetsSelector,
}
