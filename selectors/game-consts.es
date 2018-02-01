import _ from 'lodash'
import { createSelector } from 'reselect'
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

export {
  portBgmsSelector,
  mapBgmsSelector,
}
