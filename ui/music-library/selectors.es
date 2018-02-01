import { createSelector } from 'reselect'
import { uiSelector } from '../../selectors'

const musicLibrarySelector = createSelector(
  uiSelector,
  ui => ui.musicLibrary
)

const activeTabSelector = createSelector(
  musicLibrarySelector,
  ml => ml.activeTab
)

export {
  musicLibrarySelector,
  activeTabSelector,
}
