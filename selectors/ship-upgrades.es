import _ from 'lodash'
import { readJsonSync } from 'fs-extra'
import { join } from 'path-extra'
import { createSelector } from 'reselect'
import {
  constSelector,
} from 'views/utils/selectors'

import { isAbyssalShipMstId } from '../game-misc'

const shipUpgradesSelector = createSelector(
  constSelector,
  ({$shipUpgrades}) => {
    /*
       $shipUpgrades is only correct when it's an Array
       rather than Object or any falsy value.
       Previously indexfied api_mst_shipupgrade by api_id,
       but turns out api_id is not unique among elements,
       so we go back and keep it intact instead.
     */
    if (Array.isArray($shipUpgrades)) {
      /*
         elements with api_current_shid_id === 0 does not provide useful info
       */
      return $shipUpgrades.filter(x => x.api_current_ship_id !== 0)
    } else {
      console.error(`incorrect $shipUpgrades: ${$shipUpgrades}`)
      return {}
    }
  }
)

const extraCostTable = (() => {
  const xs = readJsonSync(join(__dirname, '..', 'assets', 'remodel-info-useitem.json'))
  return _.keyBy(xs, 'mstIdBefore')
})()

/*
  Returns {devMat: <number>, instantBuild: <number>, gunMat: <number>, screw: <number>}
 */
const computeExtraRemodelCost = (mstIdBefore, blueprint, steel) => {
  const info = extraCostTable[mstIdBefore]
  if (info) {
    const {
      devMatCost: devMat,
      instantBuildCost: instantBuild,
      gunMatCost: gunMat,
      screwCost: screw,
    } = info
    return {devMat, instantBuild, gunMat, screw}
  }

  /*
    This is the standard rule for deriving dev mat cost
    before all those crasy nonsense came in.
    I figure it's simplest to just maintain a lookup table.
   */
  const devMat = (blueprint > 0 || steel < 4500) ? 0 :
    steel < 5500 ? 10 :
    steel < 6500 ? 15 :
    20
  return {devMat, instantBuild: 0, gunMat: 0, screw: 0}
}

/*
   remodelDetails[mstIdBefore] = {
     mstIdBefore,
     mstIdAfter,
     level,
     ammo, steel,
     devMat, instantBuild,
     catapult, blueprint,
   }
 */
const remodelDetailsSelector = createSelector(
  constSelector,
  shipUpgradesSelector,
  ({$ships}, $shipUpgradesRaw) => {
    const remodelDetails = {}

    const $shipUpgrades = _.keyBy(
      $shipUpgradesRaw,
      'api_current_ship_id'
    )

    _.values($ships).map($ship => {
      const mstIdBefore = $ship.api_id
      if (isAbyssalShipMstId(mstIdBefore))
        return
      const mstIdAfter = Number($ship.api_aftershipid)
      if (mstIdAfter <= 0)
        return

      const ammo = $ship.api_afterbull
      // yes, this is not a typo.
      const steel = $ship.api_afterfuel

      const $shipUpgrade = $shipUpgrades[mstIdBefore]
      let extraInfo
      if (!$shipUpgrade) {
        extraInfo = {
          catapult: 0,
          blueprint: 0,
          report: 0,
          aviationMat: 0,
          armsMat: 0,
          boiler: 0,
          techMat: 0,
        }
      } else {
        extraInfo = {
          catapult: $shipUpgrade.api_catapult_count,
          blueprint: $shipUpgrade.api_drawing_count,
          report: $shipUpgrade.api_report_count,
          aviationMat: _.get($shipUpgrade, 'api_aviation_mat_count', 0),
          armsMat: _.get($shipUpgrade, 'api_arms_mat_count', 0),
          boiler: _.get($shipUpgrade, 'api_boiler_count', 0),
          techMat: _.get($shipUpgrade, 'api_tech_count', 0),
        }
      }

      remodelDetails[mstIdBefore] = {
        mstIdBefore, mstIdAfter,
        level: $ship.api_afterlv,
        ammo, steel,
        ...extraInfo,
        ...computeExtraRemodelCost(mstIdBefore, extraInfo.blueprint, steel),
      }
    })
    return remodelDetails
  }
)

export {
  remodelDetailsSelector,
}
