import _ from 'lodash'
import { modifyObject } from 'subtender'
import { splitMapId } from 'subtender/kc'
import { createSelector } from 'reselect'
import { readJsonSync } from 'fs-extra'
import { join } from 'path-extra'
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
        if (bgmId <= 0)
          return
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

/*

   Array of {area: <int>, mapIds: <Array>} sorted by area

 */
const grouppedMapIdsSelector = createSelector(
  constSelector,
  ({$maps}) =>
    _.sortBy(
      _.toPairs(
        _.groupBy(
          _.values($maps).map(x => x.api_id),
          x => splitMapId(x).area
        )
      ).map(([areaStr, mapIds]) => ({area: Number(areaStr), mapIds})),
      'area'
    )
)

const knownMapBgmIds = readJsonSync(join(__dirname, '..', 'assets', 'map-bgms.json'))

const allMapBgmIdsSelector = createSelector(
  mapBgmUseSiteInfoSelector,
  useSiteInfo =>
    _.sortBy(
      _.uniq([
        ...knownMapBgmIds,
        ..._.keys(useSiteInfo).map(Number),
      ]),
      _.identity
    )
)

const unusedMapBgmIdsSelector = createSelector(
  allMapBgmIdsSelector,
  mapBgmUseSiteInfoSelector,
  (bgmIds, useSiteInfo) =>
    bgmIds.filter(id => !(id in useSiteInfo))
)

export {
  portBgmsSelector,
  mapBgmsSelector,
  mapBgmUseSiteInfoSelector,
  grouppedMapIdsSelector,
  allMapBgmIdsSelector,
  unusedMapBgmIdsSelector,
}
