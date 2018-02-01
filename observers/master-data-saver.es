import _ from 'lodash'
import { observer } from 'redux-observers'

import { saveMasterDataFile } from '../store/ext-root/master'
import { masterSelector } from '../selectors'

const debouncedSave = _.debounce(
  mstData => setTimeout(() => saveMasterDataFile(mstData)),
  500
)

const masterDataSaver = observer(
  masterSelector,
  (_dispatch, cur, prev) => {
    // we have been careful on the reducer side,
    // so here we just need to compare by ref
    if (!_.isEmpty(cur) && cur !== prev) {
      debouncedSave(cur)
    }
  }
)

export { masterDataSaver }
