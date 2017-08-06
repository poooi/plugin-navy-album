import _ from 'lodash'
import { createSelector } from 'reselect'
import {
  swfDatabaseSelector,
} from '../../selectors'

const shipGraphSourceFuncSelector = createSelector(
  swfDatabaseSelector,
  swfDatabase =>
    (mstId, characterId, debuffFlag=false) =>
      _.get(
        swfDatabase,
        [
          'shipDb',
          mstId,
          (mstId > 1500 && debuffFlag) ?
            'imagesDebuffed' :
            'images',
          characterId,
        ]
      ) || ''
)

export { shipGraphSourceFuncSelector }
