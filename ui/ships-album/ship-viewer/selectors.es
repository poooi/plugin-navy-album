import _ from 'lodash'
import { createSelector } from 'reselect'
import { constSelector } from 'views/utils/selectors'

import {
  mstIdSelector,
  levelSelector,
  shipViewerSelector,
} from '../selectors'
import {
  remodelInfoSelector,
  wctfShipsSelector,
} from '../../../selectors'

const headerInfoSelector = createSelector(
  mstIdSelector,
  constSelector,
  (mstId,{$ships,$shipTypes}) => {
    const $ship = $ships[mstId]
    if (_.isEmpty($ship)) {
      return {
        mstId,
        shipName: '',
        typeName: '',
        yomi: '',
        ours: true,
      }
    }

    const $shipType = $shipTypes[$ship.api_stype]
    return {
      mstId,
      shipName: $ship.api_name,
      typeName: $shipType.api_name,
      yomi: $ship.api_yomi,
      ours: Boolean($ship.api_sortno),
    }
  }
)

const activeTabSelector = createSelector(
  shipViewerSelector,
  sv => sv.activeTab
)

// level-dependent stats
const statsAtCurrentLevelSelector = createSelector(
  mstIdSelector,
  levelSelector,
  wctfShipsSelector,
  (mstId, level, wctfShips) => {
    const ship = _.get(wctfShips, mstId)
    const computeStat = name => {
      const statBase = _.get(ship,['stat',name])
      const statMax = _.get(ship,['stat', `${name}_max`])
      if (
        !_.isInteger(statBase) || statBase < 0 ||
        !_.isInteger(statMax) || statMax < 0
      )
        return null
      return statBase + Math.floor((statMax - statBase)*level / 99)
    }

    return _.fromPairs(
      'los asw evasion'.split(' ').map(n =>
        [n, computeStat(n)]))
  }
)

const minLevelSelector = createSelector(
  mstIdSelector,
  remodelInfoSelector,
  constSelector,
  (mstId, {remodelChains, originMstIdOf}, {$ships}) => {
    const originMstId = originMstIdOf[mstId]
    if (! originMstId)
      return 1
    const remodelChain = remodelChains[originMstId]
    if (! remodelChain || mstId === originMstId)
      return 1
    const curInd = remodelChain.findIndex(x => x === mstId)
    const prevMstId = remodelChain[curInd-1]
    const afterLevel = _.get($ships,[prevMstId,'api_afterlv'])
    if (! _.isInteger(afterLevel) || afterLevel <= 0)
      return 1
    return afterLevel
  }
)

/*
  :TaigeiBoing:

  Ref: main.js module: CutinSSAttack._getFlagShipPosition

  Note that while for main.js it is hard-coded,
  it is reasonable to assume all AS gets this special graph.
 */
const submarineTendersSelector = createSelector(
  constSelector,
  ({$ships}) =>
    _.values($ships).flatMap(x => x.api_stype === 20 ? [x.api_id] : [])
)

export {
  headerInfoSelector,
  activeTabSelector,
  statsAtCurrentLevelSelector,
  minLevelSelector,
  submarineTendersSelector,
}
