import { readJsonSync } from 'fs-extra'
import { join } from 'path-extra'
import { createSelector } from 'reselect'

import {
  constSelector,
} from 'views/utils/selectors'

const defaultShipUpgrades = readJsonSync(
  join(__dirname, '..', 'assets', 'ship-upgrades.json')
)

const shipUpgradesSelector = createSelector(
  constSelector,
  ({$shipUpgrades}) =>
    $shipUpgrades || defaultShipUpgrades
)


/*
   - Core.swf/scripts/vo/MasterShipUpgradeData._getNeedDevNum
   - Core.swf/scripts/vo/MasterShipUpgradeData._getBuildKitNum
 */
const groupA = [503,504,508,509]
const groupB = [503,504]
const computeDevMatCount = (steel, blueprint, mstIdAfter) => {
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
  groupA.includes(mstIdAfter) ? 20 : 0

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
  ({$ships}, $shipUpgrades) => {
    const remodelDetails = {}
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

      const $shipUpgrade = $shipUpgrades[mstIdAfter]
      let extraInfo
      if (! $shipUpgrade) {
        extraInfo = {
          catapult: 0,
          blueprint: 0,
        }
      } else {
        extraInfo = {
          catapult: $shipUpgrade.api_catapult_count,
          blueprint: $shipUpgrade.api_drawing_count,
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
  shipUpgradesSelector,
  remodelDetailsSelector,
}
