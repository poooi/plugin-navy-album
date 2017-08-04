import {createSelector } from 'reselect'

import {
  uiSelector,
} from '../../selectors'

const equipmentsAlbumSelector = createSelector(
  uiSelector,
  ui => ui.equipmentsAlbum)

const listOptionsSelector = createSelector(
  equipmentsAlbumSelector,
  ea => ea.listOptions)

export {
  listOptionsSelector,
}
