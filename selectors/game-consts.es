import _ from 'lodash'
import { modifyObject, words } from 'subtender'
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
   defines the properties for map BGM as well as sorting order.
 */
const situations = words('moving normalDay normalNight bossDay bossNight')

/*
   returns: (all <bgm id> stands for map bgm)

   {
     [mapId]: {
       [situation]: <bgm id>,
     },
   }

   where situation are from `situations`

 */
const mapBgmsSelector = createSelector(
  masterSelector,
  mst => {
    const raw = _.get(mst, ['api_mst_mapbgm']) || []
    const norm = v => v <= 0 ? null : v
    return _.mapValues(_.keyBy(raw, 'api_id'), bgmRaw => ({
      moving: norm(bgmRaw.api_moving_bgm),
      normalDay: norm(bgmRaw.api_map_bgm[0]),
      normalNight: norm(bgmRaw.api_map_bgm[1]),
      bossDay: norm(bgmRaw.api_boss_bgm[0]),
      bossNight: norm(bgmRaw.api_boss_bgm[1]),
    }))
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

   - should be a subset of `situations`
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
        if (!bgmId) {
          return
        }
        useSiteInfo = modifyObject(
          bgmId,
          (uInfo = {}) =>
            modifyObject(
              mapId,
              (xs = []) => [...xs, situation]
            )(uInfo)
        )(useSiteInfo)
      }

      situations.map(propName =>
        register(bgmInfo[propName], propName)
      )
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

/*
   sortedMapBgms is an Object:

   {
     [mapId]: <Array of bgmId>, // respecting order of 'situations'
   }

 */
const sortedMapBgmsSelector = createSelector(
  mapBgmsSelector,
  mapBgms => _.mapValues(mapBgms, bgmInfo =>
    _.uniq(
      _.compact(
        situations.map(propName => bgmInfo[propName])
      )
    )
  )
)

/*
   it seems that we can actually derive this factor from store, compare the result
   of following two:

   - _.values(getStore().const.$shipTypes).map(d => d.api_scnt / 2)
   - _.values(getStore().const.$shipTypes).map(d => getDockingFactor(d.api_id))

*/
const getDockingFactorFuncSelector = createSelector(
  constSelector,
  ({$shipTypes}) => _.memoize(stype =>
    _.get($shipTypes, [stype, 'api_scnt'], NaN) / 2
  )
)

export {
  portBgmsSelector,
  mapBgmsSelector,
  mapBgmUseSiteInfoSelector,
  grouppedMapIdsSelector,
  allMapBgmIdsSelector,
  unusedMapBgmIdsSelector,
  sortedMapBgmsSelector,
  getDockingFactorFuncSelector,
}
