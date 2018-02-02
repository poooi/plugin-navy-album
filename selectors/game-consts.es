import _ from 'lodash'
import { modifyObject } from 'subtender'
import { createSelector } from 'reselect'
import { constSelector } from 'views/utils/selectors'

import { masterSelector } from './common'

/*
   returns: (all <bgm id> stands for port bgm)

   {
     [bgmId]: <string> // bgm name
   }
 */
const portBgmsSelector = createSelector(
  masterSelector,
  mst => {
    const raw = _.get(mst, ['api_mst_bgm']) || []
    return _.fromPairs(raw.map(r => [r.api_id, r.api_name]))
  }
)

/*
   returns: (all <bgm id> stands for map bgm)

   {
     [mapId]: {
       moving: <bgm id>,
       normalBattle: {day: <bgm id>, night: <bgm id>},
       bossBattle: {day: <bgm id>, night: <bgm id>},
     },
   }

 */
const mapBgmsSelector = createSelector(
  masterSelector,
  mst => {
    const raw = _.get(mst, ['api_mst_mapbgm']) || []
    const toDayNight = ([day, night]) => ({day, night})
    return _.mapValues(_.keyBy(raw, 'api_id'), bgmRaw => {
      const moving = bgmRaw.api_moving_bgm
      const normalBattle = toDayNight(bgmRaw.api_map_bgm)
      const bossBattle = toDayNight(bgmRaw.api_boss_bgm)
      return {
        moving,
        normalBattle,
        bossBattle,
      }
    })
  }
)

/*
   for figuring out things like "this bgm is used in map X for situation Y"

   returns:

   {
     [mBgmId]: <non-empty UseInfo>
   }

   UseInfo is:

   {
     [mapId]: <Array of situations>,
   }

   the situations array

   - should be a subset of:

      ['moving', 'normalDay', 'normalNight', 'bossDay', 'bossNight']

   - no duplicated element
   - element if present should be in this order.

 */
const mapBgmUseSiteInfoSelector = createSelector(
  mapBgmsSelector,
  mapBgms => {
    let useSiteInfo = {}
    _.mapValues(mapBgms, (bgmInfo, mapIdStr) => {
      const mapId = Number(mapIdStr)
      const register = (bgmId, situation) => {
        useSiteInfo = modifyObject(
          bgmId,
          (uInfo = {}) =>
            modifyObject(
              mapId,
              (xs = []) => [...xs, situation]
            )(uInfo)
        )(useSiteInfo)
      }
      register(bgmInfo.moving, 'moving')
      register(bgmInfo.normalBattle.day, 'normalDay')
      register(bgmInfo.normalBattle.night, 'normalNight')
      register(bgmInfo.bossBattle.day, 'bossDay')
      register(bgmInfo.bossBattle.night, 'bossNight')
    })
    return useSiteInfo
  }
)

export {
  portBgmsSelector,
  mapBgmsSelector,
  mapBgmUseSiteInfoSelector,
}
