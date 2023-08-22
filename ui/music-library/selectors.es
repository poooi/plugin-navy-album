import _ from 'lodash'
import { createSelector } from 'reselect'
import {
  allMapBgmIdsSelector,
  unusedMapBgmIdsSelector,
  grouppedMapIdsSelector,
  sortedMapBgmsSelector,
  uiSelector,
} from '../../selectors'

const musicLibrarySelector = createSelector(
  uiSelector,
  ui => ui.musicLibrary
)

const activeTabSelector = createSelector(
  musicLibrarySelector,
  ml => ml.activeTab
)

const mapBgmViewerSelector = createSelector(
  musicLibrarySelector,
  ml => ml.mapBgmViewer
)

const focusSelector = createSelector(
  mapBgmViewerSelector,
  m => m.focus
)

/*
   returns a function, which when applied with worldId, returns
   a sorted Array of {mapId, bgmIds: [bgmId]}
 */
const getBgmIdListByWorldFuncSelector = createSelector(
  grouppedMapIdsSelector,
  sortedMapBgmsSelector,
  (grouppedMapIds, sortedMapBgms) => _.memoize(worldId => {
    const ind = grouppedMapIds.findIndex(x => x.area === worldId)
    if (ind === -1)
      return []

    const {mapIds} = grouppedMapIds[ind]
    return mapIds.map(mapId =>
      ({
        mapId,
        bgmIds: sortedMapBgms[mapId] || [],
      })
    )
  })
)

const focusedListInfoSelector = createSelector(
  focusSelector,
  allMapBgmIdsSelector,
  unusedMapBgmIdsSelector,
  getBgmIdListByWorldFuncSelector,
  (focus, allMapBgmIds, unusedMapBgmIds, getBgmIdListByWorld) => {
    if (focus.type === 'all')
      return {type: 'simple', list: allMapBgmIds}
    if (focus.type === 'others')
      return {type: 'simple', list: unusedMapBgmIds}
    if (focus.type === 'world') {
      const {worldId} = focus
      return {type: 'groupped', list: getBgmIdListByWorld(worldId)}
    }

    console.error(`unrecognized focus type: ${focus.type}`)
    return {type: 'simple', list: []}
  }
)

export {
  musicLibrarySelector,
  activeTabSelector,
  mapBgmViewerSelector,
  focusedListInfoSelector,
  focusSelector,
}
