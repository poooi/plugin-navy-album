import _ from 'lodash'

const sumDigits = v => _.sum(String(v).split('').map(Number))

const recSumDigits = v => {
  const r = sumDigits(v)
  return r < 10 ? r : sumDigits(r)
}

const computePortBgmFileName = pBgmId => {
  const d = recSumDigits(pBgmId)
  const ch = String.fromCharCode((pBgmId * (d+1)) % 26 + 97)
  return `${pBgmId}${ch}`
}

const getPortBgmPath = pBgmId =>
  `/kcs/resources/bgm_p/${computePortBgmFileName(pBgmId)}.swf`

const getMapBgmPath = mBgmId =>
  `/kcs/resources/swf/sound_b_bgm_${mBgmId}.swf`

export {
  getPortBgmPath,
  getMapBgmPath,
}
