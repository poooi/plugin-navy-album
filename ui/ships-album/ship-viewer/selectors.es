import { createSelector } from 'reselect'
import { constSelector } from 'views/utils/selectors'

import {
  mstIdSelector,
  shipViewerSelector,
} from '../selectors'

const headerInfoSelector = createSelector(
  mstIdSelector,
  constSelector,
  (mstId,{$ships,$shipTypes}) => {
    const $ship = $ships[mstId]
    const $shipType = $shipTypes[$ship.api_stype]
    return ({
      mstId,
      shipName: $ship.api_name,
      typeName: $shipType.api_name,
    })
  }
)

const activeTabSelector = createSelector(
  shipViewerSelector,
  sv => sv.activeTab
)

export {
  headerInfoSelector,
  activeTabSelector,
}
