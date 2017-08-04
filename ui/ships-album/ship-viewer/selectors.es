import _ from 'lodash'
import { createSelector } from 'reselect'
import { constSelector } from 'views/utils/selectors'

import {
  mstIdSelector,
  levelSelector,
  shipViewerSelector,
} from '../selectors'
import { remodelInfoSelector } from '../../../selectors'
import { ships } from '../../../wctf'

const headerInfoSelector = createSelector(
  mstIdSelector,
  constSelector,
  (mstId,{$ships,$shipTypes}) => {
    const $ship = $ships[mstId]
    const $shipType = $shipTypes[$ship.api_stype]
    return ({
      mstId,
      shipName: $ship.api_name,
      typeName: $shipType.api_name,
    })
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
  (mstId, level) => {
    const ship = ships[mstId]
    const computeStat = name => {
      const statBase = _.get(ship,['stat',name])
      const statMax = _.get(ship,['stat', `${name}_max`])
      if (! _.isInteger(statBase) || ! _.isInteger(statMax))
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

export {
  headerInfoSelector,
  activeTabSelector,
  statsAtCurrentLevelSelector,
  minLevelSelector,
}
