import _ from 'lodash'
import { createSelector } from 'reselect'
import { constSelector } from 'views/utils/selectors'
import {
  serverIpSelector,
} from '../../../../selectors'
import {
  mstIdSelector,
  shipGraphSelector,
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

const voiceListSelector = createSelector(
  voiceFlagSelector,
  shipGraphSelector,
  mstIdSelector,
  serverIpSelector,
  (voiceFlag, $shipGraph, mstId, serverIp) => {
    if (! _.isInteger(voiceFlag))
      return []

    return getSituationListFromVoiceFlag(voiceFlag).map(
      ([situation,voiceId]) => {
        const path = computeVoicePath($shipGraph,mstId,voiceId)
        const url = `http://${serverIp}${path}`
        return {
          situation, voiceId, mstId,
          url,
        }
      }
    )
  }
)

export { voiceListSelector }
