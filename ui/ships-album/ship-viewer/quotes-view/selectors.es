import _ from 'lodash'
import { createSelector } from 'reselect'
import {
  constSelector,
} from 'views/utils/selectors'
import {
  serverIpSelector,
  poiConfigSelector,
} from '../../../../selectors'
import {
  mstIdSelector,
  shipGraphSelector,
  shipViewerSelector,
} from '../../selectors'

import {
  computeVoicePath,
  getSituationListFromVoiceFlag,
} from '../../../../game-misc'

const voiceFlagSelector = createSelector(
  constSelector,
  mstIdSelector,
  ({$ships}, mstId) => {
    const v = _.get($ships,[mstId,'api_voicef'])
    return _.isInteger(v) ? v : null
  }
)

const quotesOptionsSelector = createSelector(
  shipViewerSelector,
  sv => sv.quotesOptions
)

const voiceListSelector = createSelector(
  voiceFlagSelector,
  shipGraphSelector,
  mstIdSelector,
  serverIpSelector,
  quotesOptionsSelector,
  (voiceFlag, $shipGraph, mstId, serverIp, quotesOptions) => {
    if (! _.isInteger(voiceFlag))
      return []

    const {showWedding, showSunk} = quotesOptions
    return _.flatMap(
      getSituationListFromVoiceFlag(voiceFlag),
      ([situation,voiceId]) => {
        if (! showWedding && situation === 'Wedding')
          return []
        if (! showSunk && situation === 'Sunk')
          return []

        const path = computeVoicePath($shipGraph,mstId,voiceId)
        const url = `http://${serverIp}${path}`
        return [{
          situation, voiceId, mstId,
          url,
        }]
      }
    )
  }
)

const poiVolumeSelector = createSelector(
  poiConfigSelector,
  c => _.get(c,'poi.notify.volume',0.8))

export {
  voiceListSelector,
  poiVolumeSelector,
  quotesOptionsSelector,
}
