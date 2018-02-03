import _ from 'lodash'
import { createSelector } from 'reselect'
import {
  constSelector,
} from 'views/utils/selectors'
import {
  serverIpSelector,
  subtitleSelector,
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
  subtitleSelector,
  (voiceFlag, $shipGraph, mstId, serverIp, quotesOptions, subtitle) => {
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
        const line = _.get(subtitle,[mstId,voiceId]) || null
        return [{
          situation, voiceId, mstId,
          url, line,
        }]
      }
    )
  }
)

export {
  voiceListSelector,
  quotesOptionsSelector,
}
