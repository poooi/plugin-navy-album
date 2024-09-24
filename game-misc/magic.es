import _ from 'lodash'
import path from 'path-extra'

import { NavyAlbum } from '../devtools'

import {
  isAbyssalShipMstId,
} from './basic'
import {
  findHackFilePath,
  pathToFileURL,
} from './resource-hack'

const shipImgType = [
  'banner',
  /*
     technically exists for sunk ships (damaged only),
     but it's too unpleasant to put in the gallery.
   */
  // 'banner_g',
  'card',
  'remodel',
  'character_up',
  'character_full',
  'full',
  'supply_character',
  'album_status',
]

// Magic of tanaka
const resource = [
  6657, 5699, 3371, 8909, 7719, 6229, 5449, 8561, 2987, 5501,
  3127, 9319, 4365, 9811, 9927, 2423, 3439, 1865, 5925, 4409,
  5509, 1517, 9695, 9255, 5325, 3691, 5519, 6949, 5607, 9539,
  4133, 7795, 5465, 2659, 6381, 6875, 4019, 9195, 5645, 2887,
  1213, 1815, 8671, 3015, 3147, 2991, 7977, 7045, 1619, 7909,
  4451, 6573, 4545, 8251, 5983, 2849, 7249, 7449, 9477, 5963,
  2711, 9019, 7375, 2201, 5631, 4893, 7653, 3719, 8819, 5839,
  1853, 9843, 9119, 7023, 5681, 2345, 9873, 6349, 9315, 3795,
  9737, 4633, 4173, 7549, 7171, 6147, 4723, 5039, 2723, 7815,
  6201, 5999, 5339, 4431, 2911, 4435, 3611, 4423, 9517, 3243,
]

const createKey = t => {
  let e = 0
  if (t !== null && t !== '') {
    for (let i = 0; i < t.length; i++) {
      e += t.charCodeAt(i)
    }
  }
  return e
}

const create = (id, seed) => {
  const o = id.toString().match(/\d+/)
  if (o === null || o.length === 0)
    return ''
  const r = parseInt(o[0],10)
  const s = createKey(seed)
  const a = seed == null || seed.length === 0 ? 1 : seed.length
  return (17 * (r + 7) * resource[(s + r * a) % 100] % 8973 + 1e3).toString()
}

/*
  Ported from main.js VersionUtil.
 */
const mkVersionUtil = getShipGraphVersion => {
  const VersionUtil = {}
  VersionUtil.get = (versionType, id, _i = 1) => {
    if (versionType === 0) {
      const version = getShipGraphVersion(id)
      if (version) {
        return version
      }
    } else {
      /*
        TODO: Other version types include:
        - case 1: slotitem, get api_version.
        - case 2: furniture, get api_version.
        - case 3:
          + when i is 2 or 3: 母港ボイス(api_version[2])
          + otherwise ボイス(api_version[1])
        Leave them as unsupported for now, since we are only using case 0.
       */
      console.warn(`Unsupported type: ${versionType}`)
    }
    return '1'
  }
  VersionUtil.getResourceVersion = (n, o, p = 1) => {
    const q = VersionUtil.get(n, o, p)
    return q !== '1' ? `?version=${q}` : ''
  }

  return VersionUtil
}

const getHackedResourcePath = p0 => {
  // Since first character is always `/`
  const [_ignored, ...pathSegs] = _.split(p0, '/')
  const p1 = _.join(pathSegs, path.posix.sep)
  return findHackFilePath(p1)
}

const getTrueResourcePath = (p, versionPart) => {
  const hackedPath = getHackedResourcePath(p)
  if (hackedPath) {
    return pathToFileURL(hackedPath).href
  }
  return p + versionPart
}

/*
  Caches img path without a version part.
 */
const imgPathCache = new Map()

const getShipImgPathHelper = sgRaw => (id, type, damaged, debuff = false) => {
  const sgRawInfoInd = sgRaw.findIndex(x => x.api_id === id)
  const VersionUtil = mkVersionUtil(shipId => {
    const i = sgRaw.findIndex(x => x.api_id === shipId)
    return i === -1 ? null : _.get(sgRaw[i], ['api_version', 0], null)
  })

  /*
    Note that this part is not inside of the cache so that it can update at runtime
    with updates to master data.
   */
  const versionPart = VersionUtil.getResourceVersion(0, id)

  const mapkey = [id, type, damaged, debuff].toString()
  if (imgPathCache.has(mapkey)) {
    return getTrueResourcePath(imgPathCache.get(mapkey), versionPart)
  }
  /*
    TODO: `shipImgType` is used to populate gallery view,
    so we can't put `special` in there for now.
   */
  if (!shipImgType.includes(type) && type !== 'special') {
    console.warn(`unexpected type: ${type}`)
  }
  if (type === 'album_status' && damaged) {
    throw new Error('Wrong damage status!')
  }
  const ntype = type + (damaged ? '_dmg' : '')
  const seed = `ship_${ntype}`
  const cipherNum = create(id, seed)
  const padId = _.padStart(id, 4, '0')
  const debuffInfix = debuff ? '_d' : ''
  let fcukTanaka = ''
  if (type === 'full' && sgRawInfoInd !== -1) {
    fcukTanaka = `_${sgRaw[sgRawInfoInd].api_filename}`
  }
  const ret = `/kcs2/resources/ship/${ntype}/${padId}${debuffInfix}_${cipherNum}${fcukTanaka}.png`
  imgPathCache.set(mapkey, ret)
  return getTrueResourcePath(ret, versionPart)
}

NavyAlbum.getShipImgPath = (() => {
  // Note: avoiding selector to break circular deps.
  const {getStore} = window
  const sgRaw = _.get(getStore(), ['const', '$shipgraph'], [])
  return getShipImgPathHelper(sgRaw)
})()

// Reference: SlotLoader.getPath
// TODO: consider this to be a temp fix as no one is giving sgRawInp any value right now.
const getEquipImgPath = (id, type, $equipsInp = null) => {
  const fullType = `slot_${type}`
  const cip = create(id, fullType)
  const padId = _.padStart(id, 4, '0')

  const $equips = (() => {
    if ($equipsInp)
      return $equipsInp
    const {getStore} = window
    return _.get(getStore(), ['const', '$equips'], {})
  })()

  const v = _.get($equips, [id, 'api_version'], 1)
  const versionPart = v !== 1 ? `?version=${v}` : ''
  return `/kcs2/resources/slot/${type}/${padId}_${cip}.png${versionPart}`
}

NavyAlbum.getEquipImgPath = getEquipImgPath

// for non-abyssal ships only.
NavyAlbum.getAllShipImgPaths = id => {
  const {getStore} = window
  const sgRaw = _.get(getStore(), ['const', '$shipgraph'], [])
  const getShipImgPath = getShipImgPathHelper(sgRaw)

  if (isAbyssalShipMstId(id))
    throw new Error(`getAllShipImgPaths is for non-abyssal ships only`)
  const inps = [
    ['banner', false], ['banner', true],
    ['banner_g', true],
    ['card', false], ['card', true],
    ['remodel', false], ['remodel', true],
    ['character_up', false], ['character_up', true],
    ['character_full', false], ['character_full', true],
    ['full', false], ['full', true],
    ['supply_character', false], ['supply_character', true],
    ['album_status', false],
  ]

  return inps.map(([typ,dmg]) => getShipImgPath(id,typ,dmg))
}

const getBgm = (id, type) => {
  const padId = _.padStart(id, 3, '0')
  const code = create(id, `bgm_${type}`)
  return `/kcs2/resources/bgm/${type}/${padId}_${code}.mp3`
}

export {
  shipImgType,
  getShipImgPathHelper,
  getEquipImgPath,
  getBgm,
}
