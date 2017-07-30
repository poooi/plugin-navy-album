import { createSelector } from 'reselect'

import { uiSelector } from '../../selectors'

const shipAlbumSelector = createSelector(
  uiSelector,
  ui => ui.shipsAlbum)

const listOptionsSelector = createSelector(
  shipAlbumSelector,
  sa => sa.listOptions)

export {
  shipAlbumSelector,
  listOptionsSelector,
}
