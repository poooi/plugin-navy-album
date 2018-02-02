import { createSelector } from 'reselect'
import {
  allMapBgmIdsSelector, unusedMapBgmIdsSelector,
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

const focusedBgmIdListSelector = createSelector(
  focusSelector,
  allMapBgmIdsSelector,
  unusedMapBgmIdsSelector,
  (focus, allMapBgmIds, unusedMapBgmIds) => {
    if (focus.type === 'all')
      return allMapBgmIds
    if (focus.type === 'others')
      return unusedMapBgmIds
    if (focus.type === 'world') {
      // TODO
      return []
    }

    console.error(`unrecognized focus type: ${focus.type}`)
    return []
  }
)

export {
  musicLibrarySelector,
  activeTabSelector,
  mapBgmViewerSelector,
  focusedBgmIdListSelector,
}
