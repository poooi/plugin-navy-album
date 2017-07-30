import _ from 'lodash'
import { createSelector } from 'reselect'
import {
  extensionSelectorFactory,
} from 'views/utils/selectors'

const extSelector = createSelector(
  extensionSelectorFactory('poi-plugin-navy-album'),
  ext => _.isEmpty(ext) ? [] : ext)

export { extSelector }
