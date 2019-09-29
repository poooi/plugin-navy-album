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
   kcs2/js/main.js:
   - _getRequiredDevkitNum
   - _getRequiredBuildKitNum
 */
const computeDevMatCount = (steel, blueprint, mstIdBefore) => {
  const specialResults =
    // Tatsuta K2
    mstIdBefore === 214 ? 15 :
    // Saratoga Mk.II, Saratoga Mk.II Mod.2
    (mstIdBefore === 545 || mstIdBefore === 550) ? 20 :
    // Zuihou K2, Zuihou K2B
    (mstIdBefore === 555 || mstIdBefore === 560) ? 5 :
    // Hamakaze B Kai, Isokaze B Kai, Urakaze D Kai
    (mstIdBefore === 312 || mstIdBefore === 320 || mstIdBefore === 317) ? 40 :
    // Kagero K2, Shiranui K2
    (mstIdBefore === 225 || mstIdBefore === 226) ? 20 :
    // Ise K2
    mstIdBefore === 82 ? 80 :
    // Kuroshio K2
    mstIdBefore === 227 ? 20 :
    // Shiratsuyu K2
    mstIdBefore === 242 ? 15 :
    // Tenryuu K2
    mstIdBefore === 213 ? 24 :
    // Shinyou K2
    mstIdBefore === 381 ? 40 :
    // Tanikaze D Kai
    mstIdBefore === 313 ? 50 :
    // Johnston Kai
    mstIdBefore === 562 ? 80 :
    // Colorado Kai
    mstIdBefore === 149 ? 300 :
    // Akagi K2
    mstIdBefore === 277 ? 100 :
    // Akaki K2 E & Akagi K2 (back) & Fletcher Kai
    (mstIdBefore === 594 || mstIdBefore === 599 || mstIdBefore === 596) ? 80 :
    // Umikaze K2
    mstIdBefore === 350 ? 30 :
    // Janus Kai
    mstIdBefore === 520 ? 90 :
    null

  if (specialResults !== null)
    return specialResults


  const groupB = [
    // Suzuya Carrier K2
    503,
    // Kumano Carrier K2
    504,
    // Janus Kai
    520,
  ]

  if (blueprint > 0 && !groupB.includes(mstIdBefore))
    return 0
  /* eslint-disable indent */
  return steel < 4500 ? 0 :
    steel < 5500 ? 10 :
    steel < 6500 ? 15 :
    20
  /* eslint-enable indent */
}

const computeInstantBuildCount = mstIdBefore =>
  // Tatsuta K2
  mstIdBefore === 214 ? 5 :
  // Suzuya K2, Kumano K2, Suzuya Carrier K2, Kumano Carrier K2
  (mstIdBefore === 503 || mstIdBefore === 504 || mstIdBefore === 508 || mstIdBefore === 509) ? 20 :
  // Saratoga Mk.II, Saratoga Mk.II Mod.2
  (mstIdBefore === 545 || mstIdBefore === 550) ? 30 :
  // Zuihou K2, Zuihou K2B
  (mstIdBefore === 555 || mstIdBefore === 560) ? 20 :
  // Hamakaze B Kai, Isokaze B Kai, Urakaze D Kai
  (mstIdBefore === 312 || mstIdBefore === 320 || mstIdBefore === 317) ? 10 :
  // Tenryuu K2
  mstIdBefore === 213 ? 8 :
  // Tanikaze D Kai
  mstIdBefore === 313 ? 20 :
  // Johnston Kai
  mstIdBefore === 562 ? 10 :
  // Akagi K2 E && Akagi K2 (back)
  (mstIdBefore === 594 || mstIdBefore === 599) ? 30 :
  // Fletcher Kai && Janus Kai
  (mstIdBefore === 596 || mstIdBefore === 520) ? 10 :
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

    _.values($ships).map($ship => {
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
        devMat: computeDevMatCount(steel, extraInfo.blueprint, mstIdBefore),
        instantBuild: computeInstantBuildCount(mstIdBefore),
      }
    })
    return remodelDetails
  }
)

export {
  remodelDetailsSelector,
}
