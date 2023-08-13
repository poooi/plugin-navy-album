import _ from 'lodash'
import { enumFromTo } from 'subtender'
import {
  isAbyssalShipMstId,
} from './basic'

// <Core.swf>/scripts/common/util/SoundUtil.vcKey
const vcKey = [
  604825,607300,613847,615318,624009,631856,635451,637218,640529,
  643036,652687,658008,662481,669598,675545,685034,687703,696444,
  702593,703894,711191,714166,720579,728970,738675,740918,743009,
  747240,750347,759846,764051,770064,773457,779858,786843,790526,
  799973,803260,808441,816028,825381,827516,832463,837868,843091,
  852548,858315,867580,875771,879698,882759,885564,888837,896168,
]

const vcDiff = []
enumFromTo(1,53).map(voiceId => {
  vcDiff[voiceId] = vcKey[voiceId] - vcKey[voiceId-1]
})

// <Core.swf>/scripts/common/util/SoundUtil.createFileName
const computeVoiceFileName = (mstId, voiceId) => {
  if (voiceId <= 53 && !isAbyssalShipMstId(mstId)) {
    return (mstId + 7) * 17 * vcDiff[voiceId] % 99173 + 100000
  } else {
    return voiceId
  }
}

// <Core.swf>/scripts/common/util/SoundUtil.getVoiceURL
const computeVoicePath = ($shipGraph, mstId, voiceId) => {
  const shipFileName = $shipGraph.api_filename

  const voiceFileName = computeVoiceFileName(mstId, voiceId)

  const versionInd = (voiceId === 2 || voiceId === 3) ? 2 : 1
  const version = _.get($shipGraph, ['api_version', versionInd])

  const mayVersionPostfix =
    (version && version !== '1') ?
      `.mp3?version=${version}` :
      '.mp3'

  return `/kcs/sound/kc${shipFileName}/${voiceFileName}${mayVersionPostfix}`
}

const situationsCommon = [
  ['Intro',1],['Library',25],
  ['Poke(1)',2],['Poke(2)',3],['Poke(3)',4],
  ['Married',28],['Wedding',24],
  ['Ranking',8],
  ['Join',13],
  ['Equip(1)',9],['Equip(2)',10],['Equip(3)',26],
  ['Supply',27],
  ['Docking(1)',11],['Docking(2)',12],
  ['Construction',5],
  ['Return',7],['Sortie',14],['Battle',15],
  ['Attack',16],['Yasen(1)',18],['Yasen(2)',17],
  ['MVP',23],
  ['Damaged(1)',19],['Damaged(2)',20],['Damaged(3)',21],
  ['Sunk',22],
  // ['Repair',6],
]

const situationsIdle = [['Idle',29]]

const situationsHourly = enumFromTo(0,23).map(h =>
  [`H${String(h).padStart(2,'0')}00`, 30+h])

const situationsIdleSpecial = [['IdleSP',129]]

const getSituationListFromVoiceFlag = _.memoize(voiceFlag =>
  _.concat(
    situationsCommon,
    /* eslint-disable no-bitwise */
    (voiceFlag & 1) ? situationsIdle : [],
    (voiceFlag & 4) ? situationsIdleSpecial : [],
    (voiceFlag & 2) ? situationsHourly : [],
    /* eslint-enable no-bitwise */
  )
)

export {
  computeVoicePath,
  getSituationListFromVoiceFlag,
}
