import _ from 'lodash'
import { createSelector } from 'reselect'

import {
  constSelector,
} from 'views/utils/selectors'

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

/*
   - Core.swf/scripts/vo/MasterShipUpgradeData._getNeedDevNum
   - Core.swf/scripts/vo/MasterShipUpgradeData._getBuildKitNum
 */
const computeDevMatCount = (steel, blueprint, mstIdAfter) => {
  if (mstIdAfter === 545)
    return 20
  if (mstIdAfter === 550)
    return 20

  const groupB = [503,504,545,550]

  if (blueprint > 0 && !groupB.includes(mstIdAfter))
    return 0
  /* eslint-disable indent */
  return steel < 4500 ? 0 :
    steel < 5500 ? 10 :
    steel < 6500 ? 15 :
    steel < 999999 ? 20 :
    Infinity
  /* eslint-enable indent */
}

const computeInstantBuildCount = mstIdAfter =>
  // Suzuya K2
  mstIdAfter === 503 ? 20 :
  // Kumano K2
  mstIdAfter === 504 ? 20 :
  // Suzuya Carrier K2
  mstIdAfter === 508 ? 20 :
  // Kumano Carrier K2
  mstIdAfter === 509 ? 20 :
  // Saratoga Mk.II
  mstIdAfter === 545 ? 30 :
  // Saratoga Mk.II Mod.2
  mstIdAfter === 550 ? 30 :
  0

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

    Object.values($ships).map($ship => {
      const mstIdBefore = $ship.api_id
      if (mstIdBefore >= 1500)
        return
      const mstIdAfter = Number($ship.api_aftershipid)
      if (mstIdAfter <= 0)
        return

      const ammo = $ship.api_afterbull
      // yes, this is not a typo.
      const steel = $ship.api_afterfuel

      const $shipUpgrade = $shipUpgrades[mstIdBefore]
      let extraInfo
      if (! $shipUpgrade) {
        extraInfo = {
          catapult: 0,
          blueprint: 0,
          report: 0,
        }
      } else {
        extraInfo = {
          catapult: $shipUpgrade.api_catapult_count,
          blueprint: $shipUpgrade.api_drawing_count,
          report: $shipUpgrade.api_report_count,
        }
      }

      remodelDetails[mstIdBefore] = {
        mstIdBefore, mstIdAfter,
        level: $ship.api_afterlv,
        ammo, steel,
        ...extraInfo,
        devMat: computeDevMatCount(steel, extraInfo.blueprint, mstIdAfter),
        instantBuild: computeInstantBuildCount(mstIdAfter),
      }
    })
    return remodelDetails
  }
)

export {
  remodelDetailsSelector,
}
